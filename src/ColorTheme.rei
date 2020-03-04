/*
 ColorTheme.rei

 Interface for the 'colors' section of a Theme
 */

type t;

let empty: t;

/*
    [of_yojson] instantiates a TokenTheme [t] from JSON
 */
let of_yojson: Yojson.Safe.t => t;

let of_map: StringMap.t(string) => t;

let union: (t, t) => t;

let fold: ((string, string, 'a) => 'a, t, 'a) => 'a;

let getColor: (string, t) => option(string);

let getFirstOrDefault: (~default: string, list(string), t) => string;
