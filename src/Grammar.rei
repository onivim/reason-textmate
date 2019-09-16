/*
 Grammar.rei
 */

type t;

type grammarRepository = string => option(t);

let getScopeName: t => string;

let create:
  (
    ~scopeName: string,
    ~patterns: list(Pattern.t),
    ~repository: list((string, list(Pattern.t))),
    unit
  ) =>
  t;

module Json: {
  let of_yojson: Yojson.Safe.t => result(t, string);
  let of_file: string => result(t, string);
};

let tokenize:
  (
    ~lineNumber: int=?,
    ~scopes: option(ScopeStack.t)=?,
    ~grammarRepository: grammarRepository,
    ~grammar: t,
    string
  ) =>
  (list(Token.t), ScopeStack.t);
