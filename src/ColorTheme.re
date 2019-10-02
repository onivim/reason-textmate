/*
 ColorTheme.re
 */

type t = StringMap.t(string);

let empty = StringMap.empty;

let of_map = v => v;

let of_yojson = json => {
  switch (json) {
  | `Assoc(colorMap) =>
    List.fold_left(
      (prev, curr) => {
        let (colorKey, jsonValue) = curr;

        switch (jsonValue) {
        | `String(color) => StringMap.add(colorKey, color, prev)
        | _ => prev
        };
      },
      empty,
      colorMap,
    )
  | _ => empty
  };
};

let getColor = (name, v) => StringMap.find_opt(name, v);

let getFirstOrDefault = (~default, candidates, v) => {
  let rec f = curr =>
    switch (curr) {
    | [] => default
    | [hd, ...tail] =>
      switch (getColor(hd, v)) {
      | Some(col) => col
      | None => f(tail)
      }
    };

  f(candidates);
};
