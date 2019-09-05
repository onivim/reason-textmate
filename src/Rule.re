/*
 TextMateGrammar.re
 */

open Oniguruma;

type t = {
  regex: OnigRegExp.t,
  name: string,
  captures: list(Pattern.Capture.t),
  popStack: bool,
  pushStack: option((string, string)),
};

let show = (v: t) => {
  "Rule " ++ v.name;
};

let ofMatch = (match: Pattern.match_) => {
  switch (match.matchRegex) {
  | Error(_) => None
  | Ok(v) =>
    Some({
      regex: v,
      name: match.matchName,
      captures: match.captures,
      popStack: false,
      pushStack: None,
    })
  };
};

let ofMatchRangeBegin = (matchRange: Pattern.matchRange) => {
  switch (matchRange.beginRegex) {
  | Error(_) => None
  | Ok(v) =>
    Some({
      regex: v,
      name: matchRange.matchScopeName,
      captures: matchRange.beginCaptures,
      popStack: false,
      pushStack: Some((matchRange.matchScopeName, matchRange.matchRuleName)),
    })
  };
};

let ofMatchRangeEnd = (matchRange: Pattern.matchRange) => {
  switch (matchRange.endRegex) {
  | Error(_) => None
  | Ok(v) =>
    Some({
      regex: v,
      name: matchRange.matchScopeName,
      captures: matchRange.endCaptures,
      popStack: true,
      pushStack: None,
    })
  };
};

let rec ofPatterns = (~getScope, ~getFirstRangeScope, ~scopeStack, patterns) => {
  let f = (prev, pattern) => {
    switch (pattern) {
    | Pattern.Include(inc) =>
      switch (getScope(inc)) {
      | None =>
        prev;
      | Some(v) =>
        List.concat([
          ofPatterns(~getScope, ~getFirstRangeScope, ~scopeStack, v),
          prev,
        ]);
      };
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

  let patterns = List.fold_left(f, [], patterns);

  // If there is an active 'begin'/'end' rule - we need to grab the original range too
  switch (ScopeStack.activeRule(scopeStack)) {
  | None => patterns
  | Some(v) =>
    switch (getFirstRangeScope(v)) {
    | None => patterns
    | Some(matchRange) =>
      switch (ofMatchRangeEnd(matchRange)) {
      | None => patterns
      | Some(v) =>
        [v, ...patterns];
      }
    }
  };
};
