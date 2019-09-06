/*
 ScopeStack.re
 */

type scope = {
  ruleName: option(string),
  scopeName: string,
  line: int,
};

type t = list(scope);

let empty: t = [];

let ofToplevelScope = scopeName => {
  [{ruleName: None, scopeName, line: (-1)}];
};

let activeRule = (v: t) => {
  switch (v) {
  | [hd, ..._] => hd.ruleName
  | [] => None
  };
};

let pop = (v: t) => {
  switch (v) {
  | [] => v
  | [_, ...tail] => tail
  };
};

let push = (~ruleName: string, ~scopeName: string, ~line: int, v: t) => {
  [{ruleName: Some(ruleName), scopeName, line}, ...v];
};
