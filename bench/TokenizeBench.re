open Textmate;
open BenchFramework;

let reasonJson = Yojson.Safe.from_file("test/onivim/fixtures/reason.json");
let javascriptJson = Yojson.Safe.from_file("test/first-mate/fixtures/javascript.json");

let getGrammar = (json) =>
  switch (Grammar.Json.of_yojson(json)) {
  | Ok(v) => v
  | Error(msg) => failwith("Unable to load grammar: " ++ msg)
  };

let reasonGrammar = getGrammar(reasonJson);
let javascriptGrammar = getGrammar(javascriptJson);

let read_file = filename => {
  let lines = ref([]);
  let chan = open_in(filename);
  try(
    {
      while (true) {
        // print_endline("Reading line...");
        lines := [input_line(chan), ...lines^];
                                                // print_endline("Line read!");
      };
      lines^;
    }
  ) {
  | End_of_file =>
    close_in(chan);
    List.rev(lines^);
  };
};

let largeJs = Array.of_list(read_file("bench/large.js"));

let tokenizeFile = (grammar, lines, ()) => {
      let len = Array.length(lines);
      let idx = ref(0);
      let scopeStack = ref(Grammar.getScopeStack(grammar));

      while ((idx^) < len) {
          let line = lines[idx^] ++ "\n";
          let ( _, newScopeStack) =  Grammar.tokenize(
            ~lineNumber=idx^,
            ~scopes=Some(scopeStack^),
            ~grammar,
            line,
          );


        scopeStack := newScopeStack;
        incr (idx);
      };
};

let simpleTokenization = () => {
  let _ =
    Grammar.tokenize(
      ~grammar=reasonGrammar,
      ~lineNumber=0,
      ~scopes=None,
      "let add = (a, b) => a + b;",
    );
  ();
};

let setup = () => ();
let options = Reperf.Options.create(~iterations=10000, ());
let singleOption = Reperf.Options.create(~iterations=1, ());

bench(
  ~name="tokenize: simple case (reason)",
  ~options,
  ~setup,
  ~f=simpleTokenization,
  (),
);

bench(
  ~name="tokenize: Large JS file",
  ~options=singleOption,
  ~setup,
  ~f=tokenizeFile(javascriptGrammar, largeJs),
  ()
)
