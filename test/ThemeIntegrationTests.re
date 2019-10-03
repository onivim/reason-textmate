open TestFramework;

module Theme = Textmate.Theme;
module ColorTheme = Textmate.ColorTheme;
module TokenTheme = Textmate.TokenTheme;
module Scope = Textmate.ThemeScopes.Scope;
module Selector = Textmate.ThemeScopes.Selector;
module ResolvedStyle = Textmate.ThemeScopes.ResolvedStyle;
module TokenStyle = Textmate.ThemeScopes.TokenStyle;

describe("OneDark", ({test, _}) => {
  let oneDark = Theme.from_file("test/onivim/fixtures/OneDark-Pro.json");
  let oneDarkTheme = Theme.getTokenColors(oneDark);
  let oneDarkColors = Theme.getColors(oneDark);

  let darkPlus = Theme.from_file("test/onivim/fixtures/dark_plus.json");
  let darkPlusTheme = Theme.getTokenColors(darkPlus);
  let darkPlusColors = Theme.getColors(darkPlus);

  test("dark_plus - colors: load nested", ({expect, _}) => {
    // Load a color that is _only_ in parent
    switch (ColorTheme.getColor("editor.background", darkPlusColors)) {
    | None => expect.int(0).toBe(1)
    | Some(v) => expect.string(v).toEqual("#1E1E1E")
    };

    // Load a color that is _overridden_
    switch (
      ColorTheme.getColor(
        "editor.inactiveSelectionBackground",
        darkPlusColors,
      )
    ) {
    | None => expect.int(0).toBe(1)
    | Some(v) => expect.string(v).toEqual("#AABBCC")
    };
  });

  test("dark_plus - tokenColors: load nested", ({expect, _}) => {
    // Load a token that is _only_ available in parent
    let token = TokenTheme.match(darkPlusTheme, "comment");
    expect.string(token.foreground).toEqual("#6A9955");

    // Load a token that is _overridden_
    let token = TokenTheme.match(darkPlusTheme, "entity.name.label");
    expect.string(token.foreground).toEqual("#C9C9C9");
  });

  test("colors: activityBar.background", ({expect, _}) => {
    switch (ColorTheme.getColor("activityBar.background", oneDarkColors)) {
    | None => expect.int(0).toBe(1)
    | Some(v) => expect.string(v).toEqual("#282c34")
    }
  });

  test("matches multiple scopes", ({expect, _}) => {
    let token =
      TokenTheme.match(
        oneDarkTheme,
        "source.reason markup.inserted constant.language support.property-value entity.name.filename",
      );
    expect.string(token.foreground).toEqual("#d19a66");
  });

  test("c: matches include", ({expect, _}) => {
    let token =
      TokenTheme.match(
        oneDarkTheme,
        "source.c meta.preprocessor.include.c keyword.control.directive.$3.c",
      );
    expect.string(token.foreground).toEqual("#c678dd");
  });

  test("c: matches include punctuation ('#')", ({expect, _}) => {
    prerr_endline("============ BEGIN ===============");
    let token =
      TokenTheme.match(
        oneDarkTheme,
        "source.c meta.preprocessor.include.c keyword.control.directive.$3.c punctuation.definition.directive.c",
      );
    expect.string(token.foreground).toEqual("#c678dd");
    prerr_endline("============ END ===============");
  });
});
