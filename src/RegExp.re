/*
 RegExp.re

 A wrapper around Oniguruma regexps for the purpose of syntax highlighting.

 There are some cases where the raw regular expression isn't enough - for example,
 when using back-references in capture groups - these need to be generated on the fly.
 */

open Oniguruma;

type captureGroup = (int, string);

type t = {
  hasBackReferences: bool,
  captureGroups: option(list(captureGroup)),
  raw: string,
  regexp: option(OnigRegExp.t),
}

// let hasBackRefRegExp = Str.regexp("\\\\\\\\\\(\\d+\\)");
 let hasBackRefRegExp = Str.regexp("\\\\\\([0-9]+\\)");

let hasBackReferences = (v: t) => v.hasBackReferences;

let create = (regExString: string) => {
  let hasBackReferences = 
    switch(Str.search_forward(hasBackRefRegExp, regExString, 0)) {
    | exception _ => false
    | _ => true
    };

  prerr_endline ("CREATE: " ++ regExString ++ " | " ++ string_of_bool(hasBackReferences));

  switch (hasBackReferences) {
  | false => 
    let%bind regexp = OnigRegExp.create(regExString);
    Ok({
      hasBackReferences: false,
      captureGroups: None,
      raw: regExString,
      regexp: Some(regexp)
    })
  | true => Ok({
    hasBackReferences: true,
    captureGroups: None,
    raw: regExString,
    regexp: None
  });
  }
};

let emptyMatches = [||];

let search = (str: string, position: int, v: t) => {
  switch (v.regexp) {
  | Some(re) => OnigRegExp.search(str, position, re);
  | None => emptyMatches;
  }
};
