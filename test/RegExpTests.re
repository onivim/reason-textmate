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
});
