/*
 ScopeStack.re
 */

type t = {
  initialScopeName: string,
  initialPatterns: list(Pattern.t),
  scopes: list(Pattern.matchRange),
};

let ofTopLevelScope = (patterns, scopeName) => {
  {initialScopeName: scopeName, initialPatterns: patterns, scopes: []};
};

let activeRange = (v: t) => {
  switch (v.scopes) {
  | [hd, ..._] => Some(hd)
  | _ => None
  };
};

let activePatterns = (v: t) => {
  switch (v.scopes) {
  | [hd, ..._] => hd.patterns
  | [] => v.initialPatterns
  };
};

let getScopes = (v: t) => {
  List.fold_left(
    (prev, curr: Pattern.matchRange) => {[curr.matchScopeName, ...prev]},
    [v.initialScopeName],
    v.scopes,
  );
};

let pop = (v: t) => {
  let scopes =
    switch (v.scopes) {
    | [] => []
    | [_, ...tail] => tail
    };

  {...v, scopes};
};

let push = (~matchRange: Pattern.matchRange, ~line: int, v: t) => {
  ignore(line);
  let scopes = [matchRange, ...v.scopes];
  {...v, scopes};
};
