/*
 TextMateGrammar.re
 */

type t = {
  initialScopeStack: ScopeStack.t,
  scopeName: string,
  patterns: list(Pattern.t),
  repository: StringMap.t(list(Pattern.t)),
};

let getScope = (scope: string, v: t) =>
  StringMap.find_opt(scope, v.repository);

let getScopeName = (v: t) => v.scopeName;

let getFirstRangeScope = (scope: string, v: t) => {
  switch (getScope(scope, v)) {
  | Some([MatchRange(matchRange), ..._]) => Some(matchRange)
  | _ => None
  };
};

let getScopeStack = (v: t) => {
  ScopeStack.ofTopLevelScope(v.patterns, v.scopeName);
};

let create =
    (
      ~scopeName: string,
      ~patterns: list(Pattern.t),
      ~repository: list((string, list(Pattern.t))),
      (),
    ) => {
  let repositoryMap =
    List.fold_left(
      (prev, curr) => {
        let (scope, patterns) = curr;
        StringMap.add("#" ++ scope, patterns, prev);
      },
      StringMap.empty,
      repository,
    );

  let ret: t = {
    initialScopeStack: ScopeStack.ofTopLevelScope(patterns, scopeName),
    scopeName,
    patterns,
    repository: repositoryMap,
  };
  ret;
};

module Json = {
  open Yojson.Safe.Util;

  let patterns_of_yojson = (json: Yojson.Safe.t) => {
    switch (json) {
    | `List(v) =>
      List.fold_left(
        (prev, curr) => {
          switch (prev) {
          | Error(e) => Error(e)
          | Ok(currItems) =>
            switch (Pattern.Json.of_yojson(curr)) {
            | Error(e) => Error(e)
            | Ok(v) => Ok([v, ...currItems])
            }
          }
        },
        Ok([]),
        v,
      )
    | _ => Error("Patterns is expected to be a list")
    };
  };

  let repository_of_yojson = (json: Yojson.Safe.t) => {
    switch (json) {
    | `Assoc(v) =>
      List.fold_left(
        (prev, curr) => {
          switch (prev) {
          | Error(e) => Error(e)
          | Ok(currItems) =>
            let (key, json) = curr;

            // Is this a nested set of patterns?
            switch (member("begin", json), member("patterns", json)) {
            // Yes...
            | (`Null, `List(_) as patternList) =>
              let patterns = patterns_of_yojson(patternList);
              switch (patterns) {
              | Error(e) => Error(e)
              | Ok(v) => Ok([(key, v), ...currItems])
              };
            // Nope... just a single pattern
            | _ =>
              switch (Pattern.Json.of_yojson(json)) {
              | Error(e) => Error(e)
              | Ok(v) => Ok([(key, [v]), ...currItems])
              }
            };
          }
        },
        Ok([]),
        v,
      )
    | _ => Ok([])
    };
  };

  let string_of_yojson: Yojson.Safe.t => result(string, string) =
    json => {
      switch (json) {
      | `String(v) => Ok(v)
      | _ => Error("Missing expected property")
      };
    };

  let of_yojson = (json: Yojson.Safe.t) => {
    let%bind scopeName = string_of_yojson(member("scopeName", json));
    let%bind patterns = patterns_of_yojson(member("patterns", json));
    let%bind repository = repository_of_yojson(member("repository", json));

    Ok(create(~scopeName, ~patterns, ~repository, ()));
  };
};

let _getBestRule = (rules: list(Rule.t), str, position) => {
  List.fold_left(
    (prev, curr: Rule.t) => {
      let matches = RegExp.search(str, position, curr.regex);
      let matchPos = Array.length(matches) > 0 ? matches[0].startPos : (-1);

      switch (prev) {
      | None when matchPos == (-1) => None
      | None => Some((matchPos, matches, curr))
      | Some(v) =>
        let (oldMatchPos, _, _) = v;
        if (matchPos < oldMatchPos && matchPos >= position) {
          Some((matchPos, matches, curr));
        } else {
          Some(v);
        };
      };
    },
    None,
    rules,
  );
};

let tokenize = (~lineNumber=0, ~scopes=None, ~grammar: t, line: string) => {
  ignore(lineNumber);
  ignore(scopes);
  ignore(line);

  let idx = ref(0);
  let lastTokenPosition = ref(0);
  let len = String.length(line);

  let tokens = ref([]);

  let initialScope =
    switch (scopes) {
    | None => grammar.initialScopeStack
    | Some(v) => v
    };
  let scopeStack = ref(initialScope);

  while (idx^ < len) {
    let i = idx^;

    let currentScopeStack = scopeStack^;
    let patterns = ScopeStack.activePatterns(currentScopeStack);

    let rules =
      Rule.ofPatterns(
        ~getScope=v => getScope(v, grammar),
        ~scopeStack=currentScopeStack,
        patterns,
      );
    let bestRule = _getBestRule(rules, line, i);

    switch (bestRule) {
    // No matching rule... just increment position and try again
    | None => incr(idx)
    // Got a matching rule!
    | Some(v) =>
      open Oniguruma.OnigRegExp.Match;
      let (_, matches, rule) = v;
      if (Array.length(matches) > 0) {
        let ltp = lastTokenPosition^;
        let prevToken =
          if (ltp < matches[0].startPos) {
            [
              Token.create(
                ~position=ltp,
                ~length=matches[0].startPos - ltp,
                ~scopeStack=scopeStack^,
                (),
              ),
            ];
          } else {
            [];
          };

        switch (rule.pushStack) {
        // If there is nothing to push... nothing to worry about
        | None => ()
        | Some(matchRange) =>
          scopeStack :=
            ScopeStack.push(~matchRange, ~line=lineNumber, scopeStack^)
        };

        tokens :=
          [
            Token.ofMatch(~matches, ~rule, ~scopeStack=scopeStack^, ()),
            prevToken,
            ...tokens^,
          ];

        if (rule.popStack) {
          scopeStack := ScopeStack.pop(scopeStack^);
        };

        idx := matches[0].endPos;
        lastTokenPosition := matches[0].endPos;
      } else {
        incr(idx);
      };
    };
  };

  let retTokens = tokens^ |> List.rev |> List.flatten;

  let scopeStack = scopeStack^;

  (retTokens, scopeStack);
};
