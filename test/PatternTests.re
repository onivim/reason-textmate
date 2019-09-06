open TestFramework;

module Pattern = Textmate.Pattern;

describe("Pattern", ({describe, _}) => {
    describe("json parsing", ({test, _}) => {
      test("parse include pattern", ({expect, _}) => {
        let inc = Pattern.Json.of_string({|{ "include": "#value" }|});
        expect.bool(inc == Ok(Pattern.Include("#value"))).toBe(true);
        let inc2 = Pattern.Json.of_string({|{ "include": "#value2" }|});
        expect.bool(inc2 == Ok(Pattern.Include("#value2"))).toBe(true);
      });
    });
  });
