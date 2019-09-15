open Textmate;
open BenchFramework;
    
let json =
  Yojson.Safe.from_file("test/onivim/fixtures/reason.json");

let grammar = switch(Grammar.Json.of_yojson(json)) {
 | Ok(v) => v 
 | Error(msg) => failwith("Unable to load grammar: " ++ msg)
}

let simpleTokenization = () => {
    let _ = Grammar.tokenize(~grammar=grammar, ~lineNumber=0, ~scopes=None, "let add = (a, b) => a + b;")
};

let setup = () => ();
let options = Reperf.Options.create(~iterations=10000, ());

bench(
  ~name="tokenize: simple case (reason)",
  ~options,
  ~setup,
  ~f=simpleTokenization,
  (),
);
