/*
 TextMateGrammar.re
 */

open Oniguruma;

module Capture = {
  type t = (int, string);
};

  type t =
    | Include(string)
    | Match(match_)
    | MatchRange(matchRange)
  and match = {
    matchRegex: result(OnigRegExp.t, string),
    matchName: string,
    captures: list(Capture.t),
  }
  and matchRange = {
    beginRegex: result(OnigRegExp.t, string),
    endRegex: result(OnigRegExp.t, string),
    beginCaptures: list(Capture.t),
    endCaptures: list(Capture.t),
    // The scope to append to the tokens
    matchScopeName: string,
    // The rule to use when the capture group is on the top of the stack
    matchRuleName: string,
    patterns: list(t),
  };
