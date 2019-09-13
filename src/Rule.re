/*
 Rule.re
 */

type t = {
  regex: RegExp.t,
  name: option(string),
  captures: list(Pattern.Capture.t),
  popStack: option(Pattern.matchRange),
  pushStack: option(Pattern.matchRange),
};

let show = (v: t) => {
  let start =
    switch (v.name) {
    | Some(rule) => "Rule " ++ rule ++ ": "
    | None => "Rule (anonymous): "
    };

  start ++ RegExp.toString(v.regex);
};

let ofMatch = (allowA, match: Pattern.match_) => {
  Some({
    regex: RegExpFactory.compile(allowA, match.matchRegex),
    name: match.matchName,
    captures: match.captures,
    popStack: None,
    pushStack: None,
  });
};

let ofMatchRangeBegin = (allowA, matchRange: Pattern.matchRange) => {
  Some({
    regex: RegExpFactory.compile(allowA, matchRange.beginRegex),
    name: matchRange.name,
    captures: matchRange.beginCaptures,
    popStack: None,
    pushStack: Some(matchRange),
  });
};

let ofMatchRangeEnd = (allowA, matchRange: Pattern.matchRange) => {
  regex: RegExpFactory.compile(allowA, matchRange.endRegex),
  name: matchRange.name,
  captures: matchRange.endCaptures,
  popStack: Some(matchRange),
  pushStack: None,
};

let rec ofPatterns =
        (~isFirstLine, ~getScope, ~scopeStack, patterns: list(Pattern.t)) => {
  let f = (prev, pattern) => {
    switch (pattern) {
    | Pattern.Include(inc) =>
      switch (getScope(inc)) {
      | None => prev
      | Some(v) =>
        List.concat([
          ofPatterns(~isFirstLine, ~getScope, ~scopeStack, v),
          prev,
        ])
      }
    | Pattern.Match(match) =>
      switch (ofMatch(isFirstLine, match)) {
      | None => prev
      | Some(v) => [v, ...prev]
      }
    | Pattern.MatchRange(matchRange) =>
      switch (ofMatchRangeBegin(isFirstLine, matchRange)) {
      | None => prev
      | Some(v) => [v, ...prev]
      }
    };
  };

  let activeRange = ScopeStack.activeRange(scopeStack);

  let initialRules =
    switch (activeRange) {
    | Some(v) when v.applyEndPatternLast == true => [
        ofMatchRangeEnd(isFirstLine, v),
      ]
    | _ => []
    };

  let rules = List.fold_left(f, initialRules, patterns);

  switch (activeRange) {
  | Some(v) when v.applyEndPatternLast == false => [
      ofMatchRangeEnd(isFirstLine, v),
      ...rules,
    ]
  | _ => rules
  };
};
