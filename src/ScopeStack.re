/*
 ScopeStack.re
 */

open Oniguruma;

type t = {
  initialScopeName: string,
  initialPatterns: list(Pattern.t),
  patterns: list(Pattern.matchRange),
  scopes: list(string),
};

let ofTopLevelScope = (patterns, scopeName) => {
  {
    initialScopeName: scopeName,
    initialPatterns: patterns,
    scopes: [],
    patterns: [],
  };
};

let activeRange = (v: t) => {
  switch (v.patterns) {
  | [hd, ..._] => Some(hd)
  | _ => None
  };
};

let activePatterns = (v: t) => {
  switch (v.patterns) {
  | [hd, ..._] => hd.patterns
  | [] => v.initialPatterns
  };
};

let getScopes = (v: t) => {
  let scopes = List.rev(v.scopes);
  [v.initialScopeName, ...scopes] |> List.rev;
};

let popPattern = (v: t) => {
  let patterns =
    switch (v.patterns) {
    | [] => []
    | [_, ...tail] => tail
    };

  {...v, patterns};
};

let show = (v: t) => {
  let scopeStr = List.fold_left((prev, curr) => {curr ++ ", " ++ prev}, "", getScopes(v));
  let patternsStr = "[" ++ List.fold_left((prev, curr) => prev ++ "," ++ Pattern.show(Pattern.MatchRange(curr)), "", v.patterns);

  " ScopeStack: " ++ string_of_int(List.length(getScopes(v))) ++ " / " ++ string_of_int(List.length(v.patterns)) ++ "\n" ++
  " -- Scopes: " ++ scopeStr ++ "\n" ++
  " -- Patterns: " ++ patternsStr ++ "\n";
};

let pushScope = (scope: string, v: t) => {
  prerr_endline("puschScope: " ++ scope);
  let scopes = [scope, ...v.scopes];
  {...v, scopes};
};

let popScope = (v: t) => {
  prerr_endline("popScope");
  let scopes =
    switch (v.scopes) {
    | [] => []
    | [_, ...tail] => tail
    };

  {...v, scopes};
};

let pushPattern =
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

  let patterns = [newMatchRange, ...v.patterns];
  {...v, patterns};
};
