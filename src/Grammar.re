/*
 TextMateGrammar.re
 */

open Oniguruma;

type t = {
  initialScopeStack: ScopeStack.t,
  scopeName: string,
  patterns: list(Pattern.t),
  repository: StringMap.t(list(Pattern.t)),
};

let getScope = (scope: string, v: t) =>
  StringMap.find_opt(scope, v.repository);

let getFirstRangeScope = (scope: string, v: t) => {
  switch (getScope(scope, v)) {
  | Some([MatchRange(matchRange), ..._]) => Some(matchRange)
  | _ => None
  };
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
    initialScopeStack: ScopeStack.ofToplevelScope(scopeName),
    scopeName,
    patterns,
    repository: repositoryMap,
  };
  ret;
};

module Json = {

  let of_yojson = (_json: Yojson.Safe.t) => {
      ();
  };
}

let _getPatternsToMatchAgainst = (ruleName: option(string), grammar: t) => {
  let patterns =
    switch (ruleName) {
    | None => grammar.patterns
    | Some(v) =>
      switch (getFirstRangeScope(v, grammar)) {
      | None => grammar.patterns
      | Some(matchRange) => matchRange.patterns
      }
    };

  patterns;
};

let _getBestRule = (rules: list(Rule.t), str, position) => {
  List.fold_left(
    (prev, curr: Rule.t) => {
      let matches = OnigRegExp.search(str, position, curr.regex);
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
    let patterns =
      _getPatternsToMatchAgainst(
        ScopeStack.activeRule(currentScopeStack),
        grammar,
      );

    let rules =
      Rule.ofPatterns(
        ~getScope=v => getScope(v, grammar),
        ~getFirstRangeScope=v => getFirstRangeScope(v, grammar),
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
        switch (rule.pushStack) {
        // If there is nothing to push... nothing to worry about
        | None => ()
        | Some((scopeName, ruleName)) =>
          scopeStack :=
            ScopeStack.push(
              ~ruleName,
              ~scopeName,
              ~line=lineNumber,
              scopeStack^,
            )
        };

        tokens :=
          [
            Token.ofMatch(~matches, ~rule, ~scopeStack=scopeStack^, ()),
            ...tokens^,
          ];

        if (rule.popStack) {
          scopeStack := ScopeStack.pop(scopeStack^);
        };

        idx := matches[0].endPos;
      } else {
        incr(idx);
      };
    };
  };

  let retTokens = tokens^ |> List.rev |> List.flatten;

  let scopeStack = scopeStack^;

  (retTokens, scopeStack);
};
