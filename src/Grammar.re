/*
 TextMateGrammar.re
 */

type t = {
  initialScopeStack: ScopeStack.t,
  scopeName: string,
  patterns: list(Pattern.t),
  repository: StringMap.t(list(Pattern.t)),
  grammarRepository,
}
and grammarRepository = string => option(t);

let noopGrammarRepository: grammarRepository = _ => None;

let getScope = (scope: string, v: t) => {
  let len = String.length(scope);
  if (scope == "$self") {
    Some(v.patterns);
  } else if (len > 0 && scope.[0] == '#') {
    StringMap.find_opt(scope, v.repository);
  } else {
    // Raw include names without a '#' or '$' in front reference other garmmars
    switch (v.grammarRepository(scope)) {
    | Some(g) => Some(g.patterns)
    | None => None
    };
  };
};

let setGrammarRepository = (grammarRepository: grammarRepository, v: t) => {
  ...v,
  grammarRepository,
};

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
      ~grammarRepository: grammarRepository=noopGrammarRepository,
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
    grammarRepository,
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

type lastMatchedRange = option((int, Pattern.matchRange));

let _getBestRule = (lastMatchedRange, rules: list(Rule.t), str, position) => {
  let rules =
    switch (lastMatchedRange) {
    // Filter out any rule that 'pushes' or 'pops' with the same pattern we
    // had before, if we're at the same position. This prevents infinite loops,
    // where a pattern might have a non-consuming match.
    | Some((pos, matchRange)) when pos == position =>
      let filter = (rule: Rule.t) =>
        switch (rule.popStack, rule.pushStack) {
        | (Some(mr), _) when mr === matchRange => false
        | (_, Some(mr)) when mr === matchRange => false
        | _ => true
        };
      List.filter(filter, rules);
    | _ => rules
    };

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
  let lastAnchorPosition = ref(-1);
  let len = String.length(line);
  let lastMatchedRange = ref(None);

  let tokens = ref([]);

  let initialScope =
    switch (scopes) {
    | None => grammar.initialScopeStack
    | Some(v) => v
    };

  let scopeStack = ref(initialScope);

  // Iterate across the string and tokenize
  while (idx^ <= len) {
    let i = idx^;

    let currentScopeStack = scopeStack^;

    // Get active set of patterns...
    let patterns = ScopeStack.activePatterns(currentScopeStack);

    // ...and then get rules from the patterns.
    let rules =
      Rule.ofPatterns(
        ~isFirstLine=lineNumber == 0,
        ~isAnchorPos=lastAnchorPosition^ == i,
        ~getScope=v => getScope(v, grammar),
        ~scopeStack=currentScopeStack,
        patterns,
      );

    // And figure out if any of the rules applies.
    let bestRule = _getBestRule(lastMatchedRange^, rules, line, i);

    switch (bestRule) {
    // No matching rule... just increment position and try again
    | None => incr(idx)
    // Got a matching rule!
    | Some(v) =>
      open Oniguruma.OnigRegExp.Match;
      let (_, matches, rule) = v;
      let ltp = lastTokenPosition^;
      let prevToken =
        if (ltp < matches[0].startPos) {
          [
            //print_endline("Creating token at: " ++ string_of_int(ltp));
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
          ScopeStack.pushPattern(
            ~matches,
            ~matchRange,
            ~line=lineNumber,
            scopeStack^,
          );

        switch (matchRange.name) {
        | None => ()
        | Some(n) => scopeStack := ScopeStack.pushScope(n, scopeStack^)
        };
      };

      switch (rule.popStack) {
      | None => ()
      | Some(mr) =>
        switch (mr.contentName) {
        | None => ()
        | Some(_) => scopeStack := ScopeStack.popScope(scopeStack^)
        }
      };

      // Only add token if there was actually a match!
      if (matches[0].endPos > matches[0].startPos) {
        tokens :=
          [
            Token.ofMatch(~matches, ~rule, ~scopeStack=scopeStack^, ()),
            prevToken,
            ...tokens^,
          ];
        lastTokenPosition := matches[0].endPos;
      };

      switch (rule.pushStack) {
      // If there is nothing to push... nothing to worry about
      | None => ()
      | Some(matchRange) =>
        switch (matchRange.contentName) {
        | None => ()
        | Some(n) => scopeStack := ScopeStack.pushScope(n, scopeStack^)
        }
      };

      switch (rule.popStack) {
      | None => ()
      | Some(mr) =>
        scopeStack := ScopeStack.popPattern(scopeStack^);
        switch (mr.name) {
        | None => ()
        | Some(_) => scopeStack := ScopeStack.popScope(scopeStack^)
        };
      };

      let prevIndex = idx^;
      idx := max(matches[0].endPos, prevIndex);

      lastAnchorPosition := matches[0].endPos;

      let pos = idx^;
      switch (rule.popStack, rule.pushStack) {
      // If the rule isn't a push or pop rule, and we're at the same index, we're stuck
      // in a loop - we'll push forward a character in that case.
      | (None, None) when pos <= prevIndex => incr(idx)
      // Otherwise, if it's a push rule, record that we pushed so that we can break an infinite loop
      | (None, Some(mr)) => lastMatchedRange := Some((prevIndex, mr))
      | _ => ()
      };
    };
  };

  // There might be some leftover whitespace or tokens
  // that weren't processed through our loop iteration.
  let tokens =
    if (len == 0) {
      [[Token.create(~position=0, ~length=0, ~scopeStack=scopeStack^, ())]];
    } else if (lastTokenPosition^ < len) {
      [
        [
          Token.create(
            ~position=lastTokenPosition^,
            ~length=len - lastTokenPosition^,
            ~scopeStack=scopeStack^,
            (),
          ),
        ],
        ...tokens^,
      ];
    } else {
      tokens^;
    };

  let retTokens = tokens |> List.rev |> List.flatten;

  let scopeStack = scopeStack^;

  (retTokens, scopeStack);
};
