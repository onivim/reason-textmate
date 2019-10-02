/*
 ColorTheme.re
 */

type t = StringMap.t(string);

let empty = StringMap.empty;

let of_map = (v) => v;

let of_yojson = (_json) => StringMap.empty;

let getColor = (name, v) => StringMap.find_opt(name, v);

let getFirstOrDefault = (~default, candidates, v) => {
 
 let rec f = (curr) => switch(curr) {
 | [] => default
 | [hd, ...tail] => switch(getColor(hd, v)) {
   | Some(col) => col
   | None => f(tail)
  }
 }

 f(candidates);
};

