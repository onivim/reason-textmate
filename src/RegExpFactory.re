/*
 RegExpFactory.re

 A wrapper around RegExp, to handle anchors like \\A and \\G.
 */

type captureGroup = (int, string);

type anchorCache = {
  raw_A0_G0: string,
  raw_A1_G0: string,
  raw_A0_G1: string,
  raw_A1_G1: string,
};

type t = {
  hasAnchorA: bool,
  hasAnchorG: bool,
  hasBackReferences: bool,
  captureGroups: option(list(captureGroup)),
  raw: string,
  anchorCache: option(anchorCache),
  // If the regex doesn't have anchors,
  // we can just keep a ready-to-go version around.
  regex: option(RegExp.t),
};

let hasAnchorA = Str.regexp("\\\\A");
let hasAnchorG = Str.regexp("\\\\G");
let hasAnchors = (v: t) => v.hasAnchorA || v.hasAnchorG;

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

let _createAnchorCache = (str: string) => {
  let f = _ => "\\uFFFF";

  let raw_A0_G0 =
    str
    |> Str.global_substitute(hasAnchorA, f)
    |> Str.global_substitute(hasAnchorG, f);

  let raw_A0_G1 = Str.global_substitute(hasAnchorA, f, str);

  let raw_A1_G0 = Str.global_substitute(hasAnchorG, f, str);

  let raw_A1_G1 = str;

  Some({raw_A0_G1, raw_A1_G1, raw_A0_G0, raw_A1_G0});
};

let create = str => {
  let hasBackReferences =
    switch (Str.search_forward(hasBackRefRegExp, str, 0)) {
    | exception _ => false
    | _ => true
    };

  let anchorA =
    switch (Str.search_forward(hasAnchorA, str, 0)) {
    | exception _ => false
    | _ => true
    };

  let anchorG =
    switch (Str.search_forward(hasAnchorG, str, 0)) {
    | exception _ => false
    | _ => true
    };

  let anchorCache =
    if (anchorA || anchorG) {
      _createAnchorCache(str);
    } else {
      None;
    };

  {
    captureGroups: None,
    raw: str,
    regex: None,
    anchorCache,
    hasAnchorA: anchorA,
    hasAnchorG: anchorG,
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

let compile = (allowA, allowG, v: t) => {
  let rawStr =
    switch (v.anchorCache, allowA, allowG) {
    | (None, _, _) => v.raw
    | (Some({raw_A1_G1, _}), allowA, allowG)
        when allowA == true && allowG == true => raw_A1_G1
    | (Some({raw_A1_G0, _}), allowA, _) when allowA == true => raw_A1_G0
    | (Some({raw_A0_G1, _}), _, allowG) when allowG == true => raw_A0_G1
    | (Some({raw_A0_G0, _}), _, _) => raw_A0_G0
    };

  switch (v.regex) {
  | None => RegExp.create(rawStr)
  | _ => RegExp.create(rawStr)
  };
};

let show = (v: t) => v.raw;
