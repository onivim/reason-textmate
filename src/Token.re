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
    (~position, ~length, ~scope: string, ~outerScope=None,  ~scopeStack: ScopeStack.t, ()) => {
  let scopeNames = ScopeStack.getScopes(scopeStack);

  let scopes = switch(outerScope) {
  | None => [scope, ...scopeNames]
  | Some(v) => [scope, v, ...scopeNames]
  }

  let ret: t = {length, position, scopes};
  ret;
};

let show = (v: t) => {
  let scopes =
    List.fold_left((prev, curr) => prev ++ ", " ++ curr, "", v.scopes);
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
    let initialMatch = matches[0];
    let (_, tokens) = List.fold_left((prev, curr) => {
      let (pos, tokens) = prev;

      let (idx, scope) = curr;
      let match = matches[idx];

      // Was there any space between the last position and the capture?
      // If so - create a token to fill in that space
      let firstToken = if (match.startPos > pos) {
        Some(create(
        ~position=pos,
        ~length=match.startPos - pos,
        ~scope=rule.name,
        ~scopeStack,
        ()
        ))
      } else {
        None
      }

      let captureToken = create(
        ~position=match.startPos,
        ~length=match.length,
        ~scope,
        ~outerScope=Some(rule.name),
        ~scopeStack,
        (),
      );

      let tokens = switch (firstToken) {
      | Some(v) => [captureToken, v, ...tokens]
      | None => [captureToken, ...tokens];
      };

      let newPos = match.startPos + match.length;
      (newPos, tokens);
      

    }, (initialMatch.startPos, []), v);

    tokens
    |> List.filter(t => t.length > 0)
    |> List.rev;
  };
};
