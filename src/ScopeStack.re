/*
 ScopeStack.re
 */

open Oniguruma;

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

let push =
    (
      ~matches: array(OnigRegExp.Match.t),
      ~matchRange: Pattern.matchRange,
      ~line: int,
      v: t,
    ) => {
  ignore(line);

  let newMatchRange =
    if (RegExp.hasBackReferences(matchRange.endRegex)) {
      // If the end range has back references, we need to resolve them from the provided matches

      let matchGroups =
        matches
        |> Array.to_list
        |> List.map((match: OnigRegExp.Match.t) => {
             let {index, match, _}: OnigRegExp.Match.t = match;
             (index, match);
           });

      let resolvedEndRegex =
        RegExp.supplyReferences(matchGroups, matchRange.endRegex);
      {...matchRange, endRegex: resolvedEndRegex};
    } else {
      matchRange;
    };

  let scopes = [newMatchRange, ...v.scopes];
  {...v, scopes};
};
