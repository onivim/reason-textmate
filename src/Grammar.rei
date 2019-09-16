/*
 Grammar.rei
 */

type t
and grammarRepository = string => option(t);

let create:
  (
    ~scopeName: string,
    ~patterns: list(Pattern.t),
    ~repository: list((string, list(Pattern.t))),
    ~grammarRepository: grammarRepository=?,
    unit
  ) =>
  t;

module Json: {let of_yojson: Yojson.Safe.t => result(t, string);};

let tokenize:
  (
    ~lineNumber: int=?,
    ~scopes: option(ScopeStack.t)=?,
    ~grammar: t,
    string
  ) =>
  (list(Token.t), ScopeStack.t);
