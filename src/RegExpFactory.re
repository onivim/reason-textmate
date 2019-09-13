/*
 RegExpFactory.re

 A wrapper around RegExp, to handle anchors like \\A and \\G.
 */

type captureGroup = (int, string);

type t = {
  hasAnchorA: bool,
  hasBackReferences: bool,
  captureGroups: option(list(captureGroup)),
  raw: string,
  // If the regex doesn't have anchors,
  // we can just keep a ready-to-go version around.
  regex: option(RegExp.t),
};

let hasAnchorA = Str.regexp("\\A");
let hasAnchors = (v: t) => v.hasAnchorA;

let hasBackRefRegExp = Str.regexp("\\\\\\([0-9]+\\)");
let hasBackReferences = (v: t) => v.hasBackReferences;

let charactersToEscape = Str.regexp("[\\?\\,\\.\\$\\^\\+\\*{}\\\\\\|\\-]");
let additionalCharactersToEscape = Str.regexp("[][()]");
let escapeRegExpCharacters = (str: string) => {
  let f = s => "\\" ++ s;

  str
  |> Str.global_substitute(charactersToEscape, f)
  |> Str.global_substitute(additionalCharactersToEscape, f);
};

let create = str => {
  let hasBackReferences =
    switch (Str.search_forward(hasBackRefRegExp, str, 0)) {
    | exception _ => false
    | _ => true
    };

  {
    captureGroups: None,
    raw: str,
    regex: None,
    hasAnchorA: false,
    hasBackReferences,
  };
};

let supplyReferences = (references: list(captureGroup), v: t) => {
  let newRawStr =
    List.fold_left(
      (prev, curr) => {
        let (cg, text) = curr;
        let str = prev;

        let newStr =
          if (cg > 0) {
            let regexp = Str.regexp("\\\\" ++ string_of_int(cg));
            let text = escapeRegExpCharacters(text);
            Str.global_replace(regexp, text, str);
          } else {
            prev;
          };

        newStr;
      },
      v.raw,
      references,
    );

  /*let regexp =
    switch (OnigRegExp.create(newRawStr)) {
    | Ok(v) => v
    | Error(msg) =>
      failwith("Error creating regex: " ++ newRawStr ++ " - " ++ msg)
    };*/

  {...v, hasBackReferences: false, raw: newRawStr};
};

let compile = (v: t) =>
  switch (v.regex) {
  | None => RegExp.create(v.raw)
  | _ => RegExp.create(v.raw)
  };

let show = (v: t) => v.raw;
