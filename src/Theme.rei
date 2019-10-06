/*
 Theme.rei

 Interface for interacting with VSCode-flavored TextMate themes
 */

type t;

type themeLoader = string => t;

let of_yojson: (~isDark: bool=?, ~themeLoader: themeLoader, Yojson.Safe.t) => t;
let from_file: (~isDark: bool=?, string) => t;

let getColors: t => ColorTheme.t;
let getTokenColors: t => TokenTheme.t;

let isDark: t => bool;
