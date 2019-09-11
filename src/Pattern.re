/*
 TextMateGrammar.re
 */

module Capture = {
  type t = (int, string);
};

type t =
  | Include(string)
  | Match(match_)
  | MatchRange(matchRange)
and match = {
  matchRegex: RegExp.t,
  matchName: option(string),
  captures: list(Capture.t),
}
and matchRange = {
  beginRegex: RegExp.t,
  endRegex: RegExp.t,
  beginCaptures: list(Capture.t),
  endCaptures: list(Capture.t),
  // The scope to append to the tokens
  name: option(string),
  // []contentName] differs from [name] in that it only
  // impacts matches _between_ the tokens.
  contentName: option(string),
  patterns: list(t),
  applyEndPatternLast: bool,
};

let show = (v: t) =>
  switch (v) {
  | Include(str) => "Include(" ++ str ++ ")"
  | Match(_) => "Match(..)"
  | MatchRange(matchRange) =>
    let name =
      switch (matchRange.name) {
      | None => "Name: None"
      | Some(v) => "Name: " ++ v
      };
    let contentName =
      switch (matchRange.contentName) {
      | None => "ContentName: None"
      | Some(v) => "ContentName: " ++ v
      };

    "MatchRange(\n)"
    ++ " -"
    ++ name
    ++ "\n"
    ++ " -"
    ++ contentName
    ++ "\n"
    ++ " -"
    ++ RegExp.toString(matchRange.beginRegex)
    ++ "\n"
    ++ " -"
    ++ RegExp.toString(matchRange.endRegex)
    ++ "\n";
  };

module Json = {
  let string_of_yojson: (string, Yojson.Safe.t) => result(string, string) =
    (memberName, json) => {
      switch (Yojson.Safe.Util.member(memberName, json)) {
      | `String(v) => Ok(v)
      | _ => Error("Missing expected property: " ++ memberName)
      };
    };

  let bool_of_yojson: Yojson.Safe.t => bool =
    json => {
      switch (json) {
      | `Bool(v) => v
      | _ => false
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
      | `Assoc(list) => List.fold_left(fold, [], list) |> List.rev
      | _ => []
      };
    };

  let regex_of_yojson: Yojson.Safe.t => result(RegExp.t, string) =
    json => {
      switch (json) {
      | `String(v) => RegExp.create(v)
      | _ => Error("Regular expression not specified")
      };
    };

  let match_of_yojson: Yojson.Safe.t => result(t, string) =
    json => {
      open Yojson.Safe.Util;
      let%bind regex = regex_of_yojson(member("match", json));

      let nameField =
        switch (member("name", json), member("contentName", json)) {
        | (`String(_), _) => "name"
        | _ => "contentName"
        };

      let name =
        switch (string_of_yojson(nameField, json)) {
        | Ok(v) => Some(v)
        | Error(_) => None
        };

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
      let applyEndPatternLast =
        bool_of_yojson(member("applyEndPatternLast", json));

      let name =
        switch (string_of_yojson("name", json)) {
        | Ok(v) => Some(v)
        | _ => None
        };

      let contentName =
        switch (string_of_yojson("contentName", json)) {
        | Ok(v) => Some(v)
        | _ => None
        };

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

      let beginCaptureName =
        switch (member("beginCaptures", json), member("captures", json)) {
        | (`Assoc(_), _) => "beginCaptures"
        | _ => "captures"
        };

      let endCaptureName =
        switch (member("endCaptures", json), member("captures", json)) {
        | (`Assoc(_), _) => "endCaptures"
        | _ => "captures"
        };

      Ok(
        MatchRange({
          applyEndPatternLast,
          name,
          beginRegex,
          endRegex,
          beginCaptures: captures_of_yojson(member(beginCaptureName, json)),
          endCaptures: captures_of_yojson(member(endCaptureName, json)),
          contentName,
          patterns: nestedPatterns,
        }),
      );
    };

  let of_string: string => result(t, string) =
    jsonString => {
      Yojson.Safe.from_string(jsonString) |> of_yojson;
    };
};
