/**
 GrammarCaptureTests.re

 Tests specific to 'capture' behavior
*/
open TestFramework;

module Grammar = Textmate.Grammar;
module RegExp = Textmate.RegExp;

let createRegex = str => {
  switch (RegExp.create(str)) {
  | Ok(v) => v
  | Error(msg) =>
    failwith("Unable to parse regex: " ++ str ++ " message: " ++ msg)
  };
};

describe("GrammarCaptureTests", ({test, _}) => {
  let grammar =
    Grammar.create(
      ~scopeName="source.hello",
      ~patterns=[
        Match({
          matchRegex: createRegex("hello"),
          matchName: "prefix.hello",
          captures: [],
        }),
        Match({
          matchRegex: createRegex("world(!?)"),
          matchName: "suffix.hello",
          captures: [(1, "emphasis.hello")],
        }),
      ],
      ~repository=[],
      (),
    );

  test(
    "match with both name + capture gets both scopes applied", ({expect, _}) => {
    let (tokens, _) = Grammar.tokenize(~grammar, "world!");

    expect.int(List.length(tokens)).toBe(2);

    let firstToken = List.hd(tokens);
    expect.bool(firstToken.scopes == ["suffix.hello", "source.hello"]).toBe(
      true,
    );
    expect.int(firstToken.position).toBe(0);
    expect.int(firstToken.length).toBe(5);

    let secondToken = List.nth(tokens, 1);
    expect.bool(
      secondToken.scopes == ["emphasis.hello", "suffix.hello", "source.hello"],
    ).
      toBe(
      true,
    );
    expect.int(secondToken.position).toBe(5);
    expect.int(secondToken.length).toBe(1);
  });
});
