/*
 ColorTheme.re
 */

type t = StringMap.t(string);

let empty = StringMap.empty;

let of_map = (v) => v;

let of_yojson = (_json) => StringMap.empty;

let getColor = (_m) => None;

