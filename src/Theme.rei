/*
 Theme.rei

 Interface for interacting with VSCode-flavored TextMate themes
 */

type t;

let of_yojson: Yojson.Safe.t => t;
let from_file: string => t;

let getColors: t => ColorTheme.t;
let getTokenColors: t => TokenTheme.t;
