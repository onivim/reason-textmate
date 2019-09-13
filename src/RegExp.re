/*
 RegExp.re

 A wrapper around Oniguruma regexps for the purpose of syntax highlighting.

 There are some cases where the raw regular expression isn't enough - for example,
 when using back-references in capture groups - these need to be generated on the fly.
 */

open Oniguruma;

type t = {
  raw: string,
  regexp: option(OnigRegExp.t),
};

let toString = (v: t) => v.raw;

let emptyMatches = [||];

let create = (str: string) => {
  let regexp = switch (OnigRegExp.create(str)) {
  | Ok(v) => Some(v)
  | Error(msg) => failwith(msg);
    };

  {
    raw: str,
    regexp
    }
}

let search = (str: string, position: int, v: t) => {
  switch (v.regexp) {
  | Some(re) => OnigRegExp.search(str, position, re)
  | None => emptyMatches
  };
};
