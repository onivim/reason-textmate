/*
 Tokenizer.rei
 */

type t;

let create: (~repository: GrammarRepository.t, unit) => t;

let tokenize:
  (
    ~lineNumber: int=?,
    ~scopeStack: option(ScopeStack.t)=?,
    ~scope: string,
    t,
    string
  ) =>
  (list(Token.t), ScopeStack.t);
