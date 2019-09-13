open TestFramework;

module RegExpFactory = Textmate.RegExpFactory;
module RegExp = Textmate.RegExp;

let createRegex = str => {
  RegExpFactory.create(str);
};

describe("RegExpFactory", ({describe, _}) => {
  describe("hasAnchors", ({test, _}) => {
    test("returns false if no anchors", ({expect, _}) => {
      let re = createRegex("a|b|c");
      expect.bool(RegExpFactory.hasAnchors(re)).toBe(false);
    });
    test("returns true if has \\A anchor", ({expect, _}) => {
      let re = createRegex("\\A^(?!\\1(?=\\S))");
      expect.bool(RegExpFactory.hasAnchors(re)).toBe(true);
    });
  });

  describe("compile", ({test, _}) => {
    test("allowA", ({expect, _}) => {
      let re = createRegex("\\A");
      let re_a0 = RegExpFactory.compile(false, re) |> RegExp.raw;
      let re_a1 = RegExpFactory.compile(true, re) |> RegExp.raw;
      expect.string(re_a0).toEqual("\\uFFFF");
      expect.string(re_a1).toEqual("\\A");
    });
  });
  
  describe("hasBackReferences", ({test, _}) => {
    test("returns false if no backreferences", ({expect, _}) => {
      let re = createRegex("a|b|c");
      expect.bool(RegExpFactory.hasBackReferences(re)).toBe(false);
    });
    test("returns true if has backreferences", ({expect, _}) => {
      let re = createRegex("^(?!\\1(?=\\S))");
      expect.bool(RegExpFactory.hasBackReferences(re)).toBe(true);
    });
  });

  describe("supplyReferences", ({test, _}) => {
    test("back references get replaced", ({expect, _}) => {
      let re = createRegex("\\1");

      let newRe = RegExpFactory.supplyReferences([(1, "abc")], re);
      expect.bool(RegExpFactory.hasBackReferences(newRe)).toBe(false);
      expect.string(RegExpFactory.show(newRe)).toEqual("abc");
    })
  });

  describe("escapeRegExpCharacters", ({test, _}) => {
    test("escape characters get replaced", ({expect, _}) => {
      let t = RegExpFactory.escapeRegExpCharacters;

      let validate = (str, expected) => {
        let _ = expect.string(t(str)).toEqual(expected);
        ();
      };

      let cases = [
        ("|", "\\|"),
        ("-", "\\-"),
        ("\\", "\\\\"),
        ("{", "\\{"),
        ("}", "\\}"),
        ("*", "\\*"),
        ("+", "\\+"),
        ("?", "\\?"),
        ("^", "\\^"),
        ("$", "\\$"),
        (".", "\\."),
        (",", "\\,"),
        ("[", "\\["),
        ("]", "\\]"),
        ("(", "\\("),
        (")", "\\)"),
      ];

      List.iter(((e, a)) => validate(e, a), cases);
    })
  });
});
