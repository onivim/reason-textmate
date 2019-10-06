/*
 Theme.re
 */

open Rench;

type t = {
  colors: ColorTheme.t,
  tokenColors: TokenTheme.t,
  isDark: bool,
};

type themeLoader = string => t;

let of_yojson = (~isDark=?, ~themeLoader, json: Yojson.Safe.t) => {
  let parse = json => {
    let colorsJson = Yojson.Safe.Util.member("colors", json);

    let colorTheme = ColorTheme.of_yojson(colorsJson);

    let isDark =
      switch (isDark) {
      | Some(v) => v
      | None =>
        switch (Yojson.Safe.Util.member("type", json)) {
        | `String("dark") => true
        | _ => false
        }
      };

    let defaultBackground = isDark ? "#1E1E1E" : "#FFFFFF";
    let defaultForeground = isDark ? "#D4D4D4" : "#000000";

    let defaultBackground =
      ColorTheme.getFirstOrDefault(
        ~default=defaultBackground,
        ["background", "editor.background"],
        colorTheme,
      );

    let defaultForeground =
      ColorTheme.getFirstOrDefault(
        ~default=defaultForeground,
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

    // Is there an included theme? If so - we need to parse that
    let incl = Yojson.Safe.Util.member("include", json);

    let (colorTheme, tokenTheme) =
      switch (incl) {
      | `String(includePath) =>
        let parentTheme = themeLoader(includePath);

        let mergedColorTheme =
          ColorTheme.union(parentTheme.colors, colorTheme);

        let defaultBackground =
          ColorTheme.getFirstOrDefault(
            ~default="#000",
            ["background", "editor.background"],
            mergedColorTheme,
          );

        let defaultForeground =
          ColorTheme.getFirstOrDefault(
            ~default="#FFF",
            ["foreground", "editor.foreground"],
            mergedColorTheme,
          );

        let mergedTokenTheme =
          TokenTheme.union(
            ~defaultBackground,
            ~defaultForeground,
            parentTheme.tokenColors,
            tokenTheme,
          );

        (mergedColorTheme, mergedTokenTheme);
      // No 'include' - pass through as-is
      | _ => (colorTheme, tokenTheme)
      };

    {isDark, colors: colorTheme, tokenColors: tokenTheme};
  };

  parse(json);
};

let _themeCache: Hashtbl.t(string, t) = Hashtbl.create(16);

let rec from_file = (~isDark=?, path: string) => {
  switch (Hashtbl.find_opt(_themeCache, path)) {
  | Some(v) => v
  | None =>
    let currentDirectory = Path.dirname(path);
    let themeLoader = p => {
      let fullPath = Path.join(currentDirectory, p);
      from_file(fullPath);
    };
    let ret =
      Yojson.Safe.from_file(path) |> of_yojson(~isDark?, ~themeLoader);
    Hashtbl.add(_themeCache, path, ret);
    ret;
  };
};

let getColors = v => v.colors;
let getTokenColors = v => v.tokenColors;

let isDark = v => v.isDark;
