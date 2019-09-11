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

let ofMatch = (match: Pattern.match_) => {
  Some({
    regex: match.matchRegex,
    name: match.matchName,
    captures: match.captures,
    popStack: None,
    pushStack: None,
  });
};

let ofMatchRangeBegin = (matchRange: Pattern.matchRange) => {
  Some({
    regex: matchRange.beginRegex,
    name: matchRange.name,
    captures: matchRange.beginCaptures,
    popStack: None,
    pushStack: Some(matchRange),
  });
};

let ofMatchRangeEnd = (matchRange: Pattern.matchRange) => {
  regex: matchRange.endRegex,
  name: matchRange.name,
  captures: matchRange.endCaptures,
  popStack: Some(matchRange),
  pushStack: None,
};

let rec ofPatterns = (~getScope, ~scopeStack, patterns: list(Pattern.t)) => {
  let f = (prev, pattern) => {
    switch (pattern) {
    | Pattern.Include(inc) =>
      switch (getScope(inc)) {
      | None => prev
      | Some(v) => List.concat([ofPatterns(~getScope, ~scopeStack, v), prev])
      }
    | Pattern.Match(match) =>
      switch (ofMatch(match)) {
      | None => prev
      | Some(v) => [v, ...prev]
      }
    | Pattern.MatchRange(matchRange) =>
      switch (ofMatchRangeBegin(matchRange)) {
      | None => prev
      | Some(v) => [v, ...prev]
      }
    };
  };

  let activeRange = ScopeStack.activeRange(scopeStack);

  let initialRules =
    switch (activeRange) {
    | Some(v) when v.applyEndPatternLast == true => [ofMatchRangeEnd(v)]
    | _ => []
    };

  let rules = List.fold_left(f, initialRules, patterns);

  switch (activeRange) {
  | Some(v) when v.applyEndPatternLast == false => [
      ofMatchRangeEnd(v),
      ...rules,
    ]
  | _ => rules
  };
};
