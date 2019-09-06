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
  matchRegex: OnigRegExp.t,
  matchName: string,
  captures: list(Capture.t),
}
and matchRange = {
  beginRegex: OnigRegExp.t,
  endRegex: OnigRegExp.t,
  beginCaptures: list(Capture.t),
  endCaptures: list(Capture.t),
  // The scope to append to the tokens
  matchScopeName: string,
  // The rule to use when the capture group is on the top of the stack
  matchRuleName: string,
  patterns: list(t),
};

module Let_syntax = {
let bind = (~f, v) => {
  switch (v) {
  | Ok(v) => f(v)
  | Error(e) => Error(e)
  };
};

let map = (~f) =>
  fun
  | Ok(v) => Ok(f(v))
  | Error(e) => Error(e);
};

module Json = {

  let string_of_yojson: Yojson.Safe.t => result(string, string) = json => {
    switch (json) {
    | `String(v) => Ok(v)
    | _ => Error("Missing expected property")
    }

  };

  let regex_of_yojson: Yojson.Safe.t => result(OnigRegExp.t, string) = json => {
    switch (json) {
    | `String(v) => OnigRegExp.create(v)
    | _ => Error("Regular expression not specified")
    }
  };
  
  let match_of_yojson: Yojson.Safe.t => result(t, string) = (json) => {
    open Yojson.Safe.Util;
    let%bind regex = regex_of_yojson(member("match", json));
    let%bind name = string_of_yojson(member("name", json));
    
    Ok(Match({
      matchName: name,
      matchRegex: regex,
      captures: [],
    }));
  };

  /*let matchRange_of_yojson: Yojson.Safe.t => result(match_, string) => {
    
  };*/

  let of_yojson: Yojson.Safe.t => result(t, string) = (json) => {
    open Yojson.Safe.Util;
    let incl = member("include", json);
    let mat = member("match", json);
    let beg = member("begin", json);

    switch ((incl, mat, beg)) {
    | (`String(inc), _, _) => Ok(Include(inc))
    | (_, `String(_), _) => match_of_yojson(json);
    | _ => Ok(Include("#no-op"));
    }
  };

  let of_string: string => result(t, string) = (jsonString) => {
    Yojson.Safe.from_string(jsonString)
    |> of_yojson;
  };
};
