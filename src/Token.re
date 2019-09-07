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
    (
      ~position,
      ~length,
      ~scope=None,
      ~outerScope=None,
      ~scopeStack: ScopeStack.t,
      (),
    ) => {
  let scopeNames = ScopeStack.getScopes(scopeStack);

  let scopes =
    switch ((scope, outerScope)) {
    | (Some(s), Some(o)) => [s, o, ...scopeNames]
    | (Some(s), None) => [s, ...scopeNames]
    | (None, Some(o)) => [o, ...scopeNames]
    | _ => scopeNames
    };

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
        ~scope=Some(rule.name),
        ~scopeStack,
        (),
      ),
    ];
  | v =>
    let initialMatch = matches[0];
    let (_, tokens) =
      List.fold_left(
        (prev, curr) => {
          let (pos, tokens) = prev;

          let (idx, scope) = curr;
          let match = matches[idx];

          // Was there any space between the last position and the capture?
          // If so - create a token to fill in that space
          let firstToken =
            if (match.startPos > pos) {
              Some(
                create(
                  ~position=pos,
                  ~length=match.startPos - pos,
                  ~scope=Some(rule.name),
                  ~scopeStack,
                  (),
                ),
              );
            } else {
              None;
            };

          /*
               If the rule is a 'push stack', the outer rule has already been applied
               because the scope stack has been updated.
               If there rule is not a 'push stack', then we need to apply the rule here
               locally, for patterns like this:

           {
           	"match": "world(!?)",
           	"captures": {
           		"1": {
           			"name": "emphasis.hello"
           		}
           	},
           	"name": "suffix.hello"
           }

                 For a string like "world!", we'd expect two tokens:
                 - "hello" - ["suffix.hello"]
                 - "!" - ["emphasis.hello", "suffix.hello"]
               */

          let outerScope =
            switch (rule.pushStack, rule.popStack) {
            | (None, false) => Some(rule.name)
            | _ => None
            };

          let captureToken =
            create(
              ~position=match.startPos,
              ~length=match.length,
              ~scope=Some(scope),
              ~outerScope,
              ~scopeStack,
              (),
            );

          let tokens =
            switch (firstToken) {
            | Some(v) => [captureToken, v, ...tokens]
            | None => [captureToken, ...tokens]
            };

          let newPos = match.startPos + match.length;
          (newPos, tokens);
        },
        (initialMatch.startPos, []),
        v,
      );

    tokens |> List.filter(t => t.length > 0) |> List.rev;
  };
};
