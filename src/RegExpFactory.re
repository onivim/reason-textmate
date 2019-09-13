/*
 RegExpFactory.re

 A wrapper around RegExp, to handle anchors like \\A and \\G.
 */

type captureGroup = (int, string);

type anchorCache = {
    raw_A0: string,
    raw_A1: string,
}

type t = {
  hasAnchorA: bool,
  hasBackReferences: bool,
  captureGroups: option(list(captureGroup)),
  raw: string,
  anchorCache: option(anchorCache),
  // If the regex doesn't have anchors,
  // we can just keep a ready-to-go version around.
  regex: option(RegExp.t),
};

let hasAnchorA = Str.regexp("\\\\A");
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

let _createAnchorCache = (str: string) => {
  let f = _ => "\\uFFFF";
 
  let raw_A0 = Str.global_substitute(hasAnchorA, f, str);
  let raw_A1 = str;

  Some({
    raw_A0,
    raw_A1,
    })
}

let create = str => {
  let hasBackReferences =
    switch (Str.search_forward(hasBackRefRegExp, str, 0)) {
    | exception _ => false
    | _ => true
    };

  let anchorA = switch(Str.search_forward(hasAnchorA, str, 0)) {
  | exception _ => false
  | _ => true
  };

  let anchorCache = if (anchorA) {
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

let compile = (allowA, v: t) => {

  let rawStr = switch ((v.anchorCache, allowA)) {
  | (None, _) => v.raw
  | (Some({raw_A1, _}), allowA) when allowA == true => raw_A1 
  | (Some({raw_A0, _ }), _) => raw_A0
  };

  switch (v.regex) {
  | None => RegExp.create(rawStr)
  | _ => RegExp.create(rawStr)
  };
};

let show = (v: t) => v.raw;
