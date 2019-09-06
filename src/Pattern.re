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
  patterns: list(t),
};

module Json = {
  let string_of_yojson: Yojson.Safe.t => result(string, string) =
    json => {
      switch (json) {
      | `String(v) => Ok(v)
      | _ => Error("Missing expected property")
      };
    };

  let captures_of_yojson: Yojson.Safe.t => list(Capture.t) =
    json => {
      open Yojson.Safe.Util;
      let f = keyValuePair => {
        let (key, json) = keyValuePair;
        let captureGroup = int_of_string_opt(key);
        let captureName =
          switch (member("name", json)) {
          | `String(v) => Ok(v)
          | _ => Error("Capture name must be a string")
          };

        switch (captureGroup, captureName) {
        | (Some(n), Ok(name)) => Ok((n, name))
        | _ => Error("Invalid capture group")
        };
      };

      let fold = (prev, curr) => {
        switch (f(curr)) {
        | Ok(v) => [v, ...prev]
        | _ => prev
        };
      };

      switch (json) {
      | `Assoc(list) => List.fold_left(fold, [], list)
      | _ => []
      };
    };

  let regex_of_yojson: Yojson.Safe.t => result(OnigRegExp.t, string) =
    json => {
      switch (json) {
      | `String(v) => OnigRegExp.create(v)
      | _ => Error("Regular expression not specified")
      };
    };

  let match_of_yojson: Yojson.Safe.t => result(t, string) =
    json => {
      open Yojson.Safe.Util;
      let%bind regex = regex_of_yojson(member("match", json));
      let%bind name = string_of_yojson(member("name", json));

      Ok(
        Match({
          matchName: name,
          matchRegex: regex,
          captures: captures_of_yojson(member("captures", json)),
        }),
      );
    };

  let rec of_yojson: Yojson.Safe.t => result(t, string) =
    json => {
      open Yojson.Safe.Util;
      let incl = member("include", json);
      let mat = member("match", json);
      let beg = member("begin", json);

      switch (incl, mat, beg) {
      | (`String(inc), _, _) => Ok(Include(inc))
      | (_, `String(_), _) => match_of_yojson(json)
      | (_, _, `String(_)) => matchRange_of_yojson(json)
      | _ => Ok(Include("#no-op"))
      };
    }
  and matchRange_of_yojson: Yojson.Safe.t => result(t, string) =
    json => {
      open Yojson.Safe.Util;
      let%bind beginRegex = regex_of_yojson(member("begin", json));
      let%bind endRegex = regex_of_yojson(member("end", json));
      let%bind name = string_of_yojson(member("name", json));

      let%bind nestedPatterns =
        switch (member("patterns", json)) {
        | `List(items) =>
          List.fold_left(
            (prev, curr) => {
              switch (prev) {
              | Error(e) => Error(e)
              | Ok(currItems) =>
                switch (of_yojson(curr)) {
                | Ok(p) => Ok([p, ...currItems])
                | Error(e) => Error(e)
                }
              }
            },
            Ok([]),
            items,
          )
        | _ => Ok([])
        };

      Ok(
        MatchRange({
          matchScopeName: name,
          beginRegex,
          endRegex,
          beginCaptures: captures_of_yojson(member("beginCaptures", json)),
          endCaptures: captures_of_yojson(member("endCaptures", json)),
          patterns: nestedPatterns,
        }),
      );
    };

  let of_string: string => result(t, string) =
    jsonString => {
      Yojson.Safe.from_string(jsonString) |> of_yojson;
    };
};
