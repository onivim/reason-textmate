/*

  FirstMateTests.re

  The 'first-mate' test-suite is a suite of tests that came from https://github.com/microsoft/vscode-textmate,
  which was generated from another set of tests from Atom - https://github.com/atom/first-mate
 */

open TestFramework;

module Grammar = Textmate.Grammar;
module Token = Textmate.Token;
module StringMap =
  Map.Make({
    type t = string;
    let compare = String.compare;
  });

module FirstMateTest = {
  [@deriving yojson({strict: false})]
  type expectedToken = {
    value: string,
    scopes: list(string),
  };

  [@deriving yojson({strict: false})]
  type line = {
    line: string,
    tokens: list(expectedToken),
  };

  [@deriving yojson({strict: false})]
  type t = {
    [@default None]
    grammarScopeName: option(string),
    [@default None]
    grammarPath: option(string),
    grammars: list(string),
    lines: list(line),
    desc: string,
  };

  let _loadGrammar = (grammarPath: string) => {
    let json = Yojson.Safe.from_file("test/first-mate/" ++ grammarPath);
    switch (Grammar.Json.of_yojson(json)) {
    | Ok(v) => v
    | Error(msg) =>
      failwith("Unable to load grammar " ++ grammarPath ++ ": " ++ msg)
    };
  };

  let _loadGrammars = (v: t) => {
    open Grammar;

    let allGrammars =
      switch (v.grammarPath) {
      | Some(g) => [g, ...v.grammars]
      | None => v.grammars
      };

    List.fold_left(
      (prev, curr) => {
        let grammar = _loadGrammar(curr);
        let scopeName = grammar.scopeName;

        StringMap.add(scopeName, grammar, prev);
      },
      StringMap.empty,
      allGrammars,
    );
  };

  let run = (pass, fail, v: t) => {
    ignore(fail);

    let grammarMap = _loadGrammars(v);
    prerr_endline("Loaded grammars!");
    pass("Grammars loaded");

    let grammar =
      switch (v.grammarPath) {
      | Some(p) => _loadGrammar(p)
      | None =>
        switch (v.grammarScopeName) {
        | Some(s) => StringMap.find(s, grammarMap)
        | None => failwith("Unable to locate grammar")
        }
      };

    let idx = ref(0);
    let linesArray = Array.of_list(v.lines);
    let len = Array.length(linesArray);
    let scopeStack = ref(Grammar.getScopeStack(grammar));

    while (idx^ < len) {
      let l = linesArray[idx^];

      prerr_endline(
        "Tokenizing line: " ++ string_of_int(idx^) ++ "|" ++ l.line ++ "|",
      );
      let scopes = scopeStack^;
      let (tokens, newScopeStack) =
        Grammar.tokenize(~scopes=Some(scopes), ~grammar, l.line);

      List.iter(t => prerr_endline(Token.show(t)), tokens);
      scopeStack := newScopeStack;

      incr(idx);
    };
  };
};

module FirstMateTestSuite = {
  [@deriving yojson({strict: false})]
  type t = list(FirstMateTest.t);

  let ofFile = (filePath: string) => {
    let json = Yojson.Safe.from_file(filePath);
    switch (of_yojson(json)) {
    | Ok(v) => v
    | Error(msg) => failwith("Unable to load " ++ filePath ++ ": " ++ msg)
    };
  };

  let run = (runTest, v: t) => {
    prerr_endline("Found " ++ string_of_int(List.length(v)) ++ " cases");

    List.iter(
      (t: FirstMateTest.t) => {
        runTest(t.desc, (pass, fail) => {FirstMateTest.run(pass, fail, t)})
      },
      v,
    );
  };
};

let getExecutingDirectory = () => {
  Filename.dirname(Sys.argv[0]);
};

describe("FirstMate", ({test, _}) => {
  // We'll load and parse the JSON
  let testSuite = FirstMateTestSuite.ofFile("test/first-mate/tests.json");

  let runTest = (name, f) => {
    test(
      name,
      ({expect, _}) => {
        let fail = msg => failwith(msg);
        let pass = msg => expect.string(msg).toEqual(msg);

        f(pass, fail);
      },
    );
  };

  FirstMateTestSuite.run(runTest, testSuite);
});
