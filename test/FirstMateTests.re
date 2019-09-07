/*
 
 FirstMateTests.re

 The 'first-mate' test-suite is a suite of tests that came from https://github.com/microsoft/vscode-textmate,
 which was generated from another set of tests from Atom - https://github.com/atom/first-mate
*/

open TestFramework;

module Grammar = Textmate.Grammar;
module Token = Textmate.Token;

module FirstMateTest = {

  [@deriving yojson({strict: false})]
  type expectedToken = {
    value: string,
    scopes: list(string),
  };

  [@deriving yojson({strict: false})]
  type line = {
    line: string,
    tokens: list(expectedToken)
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
}

module FirstMateTestSuite = {

  [@deriving yojson({strict: false})]
  type t = list(FirstMateTest.t);
  
  let ofFile = (filePath: string) => {
      let json = Yojson.Safe.from_file(filePath);
      switch(of_yojson(json)) {
      | Ok(v) => v
      | Error(msg) => failwith("Unable to load " ++ filePath ++ ": " ++ msg);
      }
  };

  let run = (describe, v: t) => {
      ignore(describe);
      prerr_endline ("Found " ++ string_of_int(List.length(v)) ++ " cases");
  };
};

let getExecutingDirectory = () => {
  Filename.dirname(Sys.argv[0]);
};

describe("FirstMate", ({describe, _}) => {

  // We'll load and parse the JSON
  let testSuite = FirstMateTestSuite.ofFile("test/first-mate/tests.json");

  FirstMateTestSuite.run(describe, testSuite);
});
