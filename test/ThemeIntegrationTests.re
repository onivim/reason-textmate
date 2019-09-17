open TestFramework;

module Theme = Textmate.Theme;
module Scope = Textmate.ThemeScopes.Scope;
module Selector = Textmate.ThemeScopes.Selector;
module ResolvedStyle = Textmate.ThemeScopes.ResolvedStyle;
module TokenStyle = Textmate.ThemeScopes.TokenStyle;

describe("OneDark", ({test, _}) => {
  let oneDarkJson =
    Yojson.Safe.from_file("test/onivim/fixtures/OneDark-Pro.json");
  let oneDarkTokens = Yojson.Safe.Util.member("tokenColors", oneDarkJson);
  let oneDarkTheme =
    Theme.of_yojson(
      ~defaultBackground="#000",
      ~defaultForeground="#FFF",
      oneDarkTokens,
    );

  test("matches multiple scopes", ({expect, _}) => {
    let token =
      Theme.match(
        oneDarkTheme,
        "source.reason markup.inserted constant.language support.property-value entity.name.filename",
      );
    expect.string(token.foreground).toEqual("#d19a66");
  });

  test("c: matches include", ({expect, _}) => {
    let token =
      Theme.match(
        oneDarkTheme,
        "source.c meta.preprocessor.include.c keyword.control.directive.$3.c",
      );
    expect.string(token.foreground).toEqual("#c678dd");
  });

  test("c: matches include punctuation ('#')", ({expect, _}) => {
    prerr_endline("============ BEGIN ===============");
    let token =
      Theme.match(
        oneDarkTheme,
        "source.c meta.preprocessor.include.c keyword.control.directive.$3.c punctuation.definition.directive.c",
      );
    expect.string(token.foreground).toEqual("#c678dd");
    prerr_endline("============ END ===============");
  });
});
