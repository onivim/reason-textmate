open TestFramework;

module Pattern = Textmate.Pattern;

describe("Pattern", ({describe, _}) => {
    describe("json parsing", ({test, _}) => {
      test("include", ({expect, _}) => {
        let inc = Pattern.Json.of_string({|{ "include": "#value" }|});
        expect.bool(inc == Ok(Pattern.Include("#value"))).toBe(true);
        let inc2 = Pattern.Json.of_string({|{ "include": "#value2" }|});
        expect.bool(inc2 == Ok(Pattern.Include("#value2"))).toBe(true);
      });
      test("match", ({expect, _}) => {
        let match1 = Pattern.Json.of_string({|{ "match": "a|b|c", name: "match1" }|});

        switch (match1) {
        | Ok(Match(v)) =>
          expect.string(v.matchName).toEqual("match1");
          expect.int(List.length(v.captures)).toBe(0);
        | _ => failwith("Parse failed for match");
        }
        
        let matchWithCapture = Pattern.Json.of_string({|{ "match": "a|b|c", name: "match2", captures: { "0": "derp" } }|});

        switch (matchWithCapture) {
        | Ok(Match(v)) =>
          expect.string(v.matchName).toEqual("match2");
          expect.int(List.length(v.captures)).toBe(1);
        | _ => failwith("Parse failed for match");
        }
      });
    });
  });
