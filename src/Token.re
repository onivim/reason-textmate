/*
 Token.re
 */

open Oniguruma;

type t = {
  position: int,
  length: int,
  scopes: list(string),
};

let create =
    (~position, ~length, ~scope: string, ~scopeStack: ScopeStack.t, ()) => {
  let scopeNames =
    List.map((s: ScopeStack.scope) => s.scopeName, scopeStack);

  let ret: t = {length, position, scopes: [scope, ...scopeNames]};
  ret;
};

let show = (v: t) => {
  let scopes =
    List.fold_left((prev, curr) => prev ++ "." ++ curr, "", v.scopes);
  "Token("
  ++ string_of_int(v.position)
  ++ ","
  ++ string_of_int(v.position + v.length)
  ++ ":"
  ++ scopes
  ++ ")";
};

let ofMatch =
    (
      ~matches: array(OnigRegExp.Match.t),
      ~rule: Rule.t,
      ~scopeStack: ScopeStack.t,
      (),
    ) => {
  switch (rule.captures) {
  | [] =>
    let match = matches[0];
    [
      create(
        ~position=match.startPos,
        ~length=match.length,
        ~scope=rule.name,
        ~scopeStack,
        (),
      ),
    ];
  | v =>
    List.map(
      cap => {
        let (idx, scope) = cap;
        let match = matches[idx];
        create(
          ~position=match.startPos,
          ~length=match.length,
          ~scope,
          ~scopeStack,
          (),
        );
      },
      v,
    )
  };
};
