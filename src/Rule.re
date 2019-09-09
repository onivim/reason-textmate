/*
 Rule.re
 */

type t = {
  regex: RegExp.t,
  name: string,
  captures: list(Pattern.Capture.t),
  popStack: bool,
  pushStack: option(Pattern.matchRange),
};

let show = (v: t) => {
  "Rule " ++ v.name;
};

let ofMatch = (match: Pattern.match_) => {
  Some({
    regex: match.matchRegex,
    name: match.matchName,
    captures: match.captures,
    popStack: false,
    pushStack: None,
  });
};

let ofMatchRangeBegin = (matchRange: Pattern.matchRange) => {
  Some({
    regex: matchRange.beginRegex,
    name: matchRange.matchScopeName,
    captures: matchRange.beginCaptures,
    popStack: false,
    pushStack: Some(matchRange),
  });
};

let ofMatchRangeEnd = (matchRange: Pattern.matchRange) => {
  regex: matchRange.endRegex,
  name: matchRange.matchScopeName,
  captures: matchRange.endCaptures,
  popStack: true,
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

  let initialList =
    switch (ScopeStack.activeRange(scopeStack)) {
    | Some(v) => [ofMatchRangeEnd(v)]
    | None => []
    };

  List.fold_left(f, initialList, patterns);
};
