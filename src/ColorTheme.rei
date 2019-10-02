/*
 ColorTheme.rei

 Interface for the 'colors' section of a Theme
 */

type t;

let empty: t;

/*
    [of_yojson] instantiates a TokenTheme [t] from JSON
 */
let of_yojson:
  (Yojson.Safe.t) => t;

let of_map: (StringMap.t(string)) => t;

let getColor: string => option(string);
