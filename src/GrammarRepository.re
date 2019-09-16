/*
 GrammarRepository.re
 */

type grammarRepository = string => option(Grammar.t);
type t = grammarRepository;

let getGrammar = (repository, scope) => repository(scope);

let ofGrammar = (scope, grammar, s) =>
  switch (s) {
  | v when v == scope => Some(grammar)
  | _ => None
  };

let create = v => v;
