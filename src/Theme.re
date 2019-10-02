/*
 Theme.re
 */

type t = {
  colors: ColorTheme.t,
  tokenColors: TokenTheme.t,
};

let of_yojson = (json: Yojson.Safe.t) => {
  let colorsJson = Yojson.Safe.Util.member("colors", json);

  let colorTheme = ColorTheme.of_yojson(colorsJson);

  let defaultBackground =
    ColorTheme.getFirstOrDefault(
      ~default="#000",
      ["background", "editor.background"],
      colorTheme,
    );

  let defaultForeground =
    ColorTheme.getFirstOrDefault(
      ~default="#FFF",
      ["foreground", "editor.foreground"],
      colorTheme,
    );

  let tokenColorsJson = Yojson.Safe.Util.member("tokenColors", json);

  let tokenTheme =
    TokenTheme.of_yojson(
      ~defaultBackground,
      ~defaultForeground,
      tokenColorsJson,
    );

  {colors: colorTheme, tokenColors: tokenTheme};
};

let from_file = (path: string) => {
  Yojson.Safe.from_file(path) |> of_yojson;
};

let getColors = v => v.colors;
let getTokenColors = v => v.tokenColors;
