open TestFramework;

module RegExp = Textmate.RegExp;

let createRegex = str => {
  switch (RegExp.create(str)) {
  | Ok(v) => v
  | Error(msg) =>
    failwith("Unable to parse regex: " ++ str ++ " message: " ++ msg)
  };
};

describe("RegExp", ({describe, _}) => {
  describe("hasBackReferences", ({test, _}) => {
    test("returns false if no backreferences", ({expect, _}) => {
      let re = createRegex("a|b|c");
      expect.bool(RegExp.hasBackReferences(re)).toBe(false);
    });
    test("returns true if has backreferences", ({expect, _}) => {
      let re = createRegex("^(?!\\1(?=\\S))");
      expect.bool(RegExp.hasBackReferences(re)).toBe(true);
    });
  });

  describe("supplyReferences", ({test, _}) => {
    test("back references get replaced", ({expect, _}) => {
      let re = createRegex("\\1");

      let newRe = RegExp.supplyReferences([(1, "abc")], re);
      expect.bool(RegExp.hasBackReferences(newRe)).toBe(false);
      expect.string(RegExp.toString(newRe)).toEqual("abc");
    })
  });

  describe("escapeRegExpCharacters", ({test, _}) => {
      test("escape characters get replaced", ({expect, _}) => {
        let t = RegExp.escapeRegExpCharacters;

        let validate = (str, expected) => {
         let _ = expect.string(t(str)).toEqual(expected);
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
        (")", "\\)")
        ];

        List.iter(((e, a)) => validate(e, a), cases);
      });
    });
});
