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
    prerr_endline("STARTING");
    let token =
      Theme.match(
        oneDarkTheme,
        "entity.name.filename support.property-value constant.language markup.inserted source.reason",
      );
    expect.string(token.foreground).toEqual("#d19a66");
    prerr_endline("ENDING");
  });
});
