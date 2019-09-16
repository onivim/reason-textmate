/*
 GrammarRepository.rei
 */

type t;

let getGrammar: (t, string) => option(Grammar.t);

let ofGrammar: (string, Grammar.t) => t;
let ofFilePath: (string, string) => t;

type grammarRepository = string => option(Grammar.t);

let create: grammarRepository => t;
