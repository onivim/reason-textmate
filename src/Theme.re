/*
 Theme.re
 */

open ThemeScopes;

module TokenStyle = ThemeScopes.TokenStyle;
module ResolvedStyle = ThemeScopes.ResolvedStyle;

type themeSelector = (string, TokenStyle.t);

type selectorWithParents = {
  style: option(TokenStyle.t),
  parents: list(Selector.t),
};

type t = {
  defaultBackground: string,
  defaultForeground: string,
  trie: Trie.t(selectorWithParents),
};

/* Helper to split the selectors on ',' for groups */
let _explodeSelectors = (s: string) => {
  s
  |> String.split_on_char(',')
  |> List.map(s => String.trim(s))
  |> List.filter(s =>
       switch (String.index_opt(s, '>')) {
       | None => true
       | Some(_) => false
       }
     );
};

let create =
    (~defaultBackground, ~defaultForeground, selectors: list(themeSelector)) => {
  let f = (v: themeSelector) => {
    let (s, style) = v;

    // Expand "foo, bar" -> ["foo", "bar"]
    let explodedSelectors = _explodeSelectors(s);

    List.map(
      scope => Selector.create(~style, ~scopes=Scopes.ofString(scope), ()),
      explodedSelectors,
    );
  };
  let selectors = selectors |> List.map(f) |> List.flatten;

  let trie =
    List.fold_left(
      (prev, curr) => {
        open Selector;
        let {scopes, style} = curr;

        let revScopes = List.rev(scopes);
        switch (revScopes) {
        | [] => prev
        | [hd] =>
          // If this is the only node (no parent selector),
          // we just want to set the selector directly on the node
          let f = prev =>
            switch (prev) {
            | None => Some({parents: [], style: Some(style)})
            | Some(v) => Some({...v, style: Some(style)})
            };

          Trie.update(hd, f, prev);
        | [hd, ...tail] =>
          let f = prev =>
            switch (prev) {
            | None =>
              Some({
                // TODO: Change this to have None
                style: None,
                parents: [{style, scopes: tail}],
              })
            | Some(v) =>
              Some({...v, parents: [{style, scopes: tail}, ...v.parents]})
            };

          Trie.update(hd, f, prev);
        };
      },
      Trie.empty,
      selectors,
    );

  let ret: t = {defaultBackground, defaultForeground, trie};

  ret;
};

let of_yojson = (~defaultBackground, ~defaultForeground, json: Yojson.Safe.t) => {
  let parseSettings: Yojson.Safe.t => TokenStyle.t =
    json => {
      let str = v =>
        switch (v) {
        | `String(s) => Some(s)
        | _ => None
        };

      let boo = v =>
        switch (v) {
        | `Bool(s) => Some(s)
        | _ => None
        };

      TokenStyle.create(
        ~foreground=str(Yojson.Safe.Util.member("foreground", json)),
        ~background=str(Yojson.Safe.Util.member("background", json)),
        ~bold=boo(Yojson.Safe.Util.member("bold", json)),
        ~italic=boo(Yojson.Safe.Util.member("italic", json)),
        (),
      );
    };

  let parseStringList = (arr: list(Yojson.Safe.t)) => {
    List.fold_left(
      (prev, curr) =>
        switch (curr) {
        | `String(v) => [v, ...prev]
        | _ => prev
        },
      [],
      arr,
    );
  };

  let parseSelector = (selector: Yojson.Safe.t) => {
    switch (selector) {
    | `Assoc(_) =>
      let scope = Yojson.Safe.Util.member("scope", selector);
      let settings = Yojson.Safe.Util.member("settings", selector);

      switch (scope, settings) {
      | (`List(v), `Assoc(_)) =>
        let tokenStyle = parseSettings(settings);
        let selector = parseStringList(v) |> String.concat(",");
        [(selector, tokenStyle)];
      | (`String(v), `Assoc(_)) =>
        let tokenStyle = parseSettings(settings);
        let selector = v;
        [(selector, tokenStyle)];
      | _ => []
      };
    | _ => []
    };
  };

  let selectors =
    switch (json) {
    | `List(elems) => List.map(parseSelector, elems) |> List.flatten
    | _ => []
    };

  create(~defaultBackground, ~defaultForeground, selectors);
};

let empty = create(~defaultBackground="#000", ~defaultForeground="#fff", []);

let show = (v: t) => {
  Trie.show(
    (i: selectorWithParents) => {
      switch (i.style) {
      | Some(v) => TokenStyle.show(v)
      | None => "(None)"
      }
    },
    v.trie,
  );
};

let _applyStyle: (TokenStyle.t, TokenStyle.t) => TokenStyle.t =
  (prev: TokenStyle.t, style: TokenStyle.t) => {
    let foreground =
      switch (prev.foreground, style.foreground) {
      | (Some(v), _) => Some(v)
      | (_, Some(v)) => Some(v)
      | _ => None
      };

    let background =
      switch (prev.background, style.background) {
      | (Some(v), _) => Some(v)
      | (_, Some(v)) => Some(v)
      | _ => None
      };

    let bold =
      switch (prev.bold, style.bold) {
      | (Some(v), _) => Some(v)
      | (_, Some(v)) => Some(v)
      | _ => None
      };

    let italic =
      switch (prev.italic, style.italic) {
      | (Some(v), _) => Some(v)
      | (_, Some(v)) => Some(v)
      | _ => None
      };

    {background, foreground, bold, italic};
  };

let match = (theme: t, scopes: string) => {
  let scopes = Scopes.ofString(scopes) |> List.rev;
  let default =
    ResolvedStyle.default(
      ~foreground=theme.defaultForeground,
      ~background=theme.defaultBackground,
      (),
    );

  let rec f = scopes => {
    switch (scopes) {
    | [] => default
    | [scope, ...scopeParents] =>
      let p = Trie.matches(theme.trie, scope);

      prerr_endline(
        "Checking scope: "
        ++ String.concat("|", scope)
        ++ " trie length: "
        ++ string_of_int(List.length(p)),
      );
      // If there were no matches... try the next scope up.
      switch (p) {
      | [] => f(scopeParents)
      | _ =>
        let result =
          List.fold_left(
            (prev: option(TokenStyle.t), curr) => {
              let (name, selector: option(selectorWithParents)) = curr;

              switch (selector) {
              | None => prev
              | Some({style, parents}) =>
                prerr_endline(
                  "Got a selector?? "
                  ++ name
                  ++ string_of_int(List.length(parents)),
                );
                let prevStyle =
                  switch (prev) {
                  | None => TokenStyle.default
                  | Some(v) => v
                  };

                let parentsScopesToApply =
                  parents
                  |> List.filter(selector =>
                       Selector.matches(selector, scopeParents)
                     );

                switch (parentsScopesToApply, style) {
                | ([], None) => None
                | ([], Some(style)) => Some(_applyStyle(prevStyle, style))
                | (_, style) =>
                  let newStyle =
                    switch (style) {
                    | Some(v) => _applyStyle(prevStyle, v)
                    | None => TokenStyle.default
                    };
                  // Apply any parent selectors that match...
                  // we should be sorting this by score!
                  Some(
                    List.fold_left(
                      (prev, curr: Selector.t) => {
                        open Selector;
                        let {style, _} = curr;
                        // Reversing the order because the parent style
                        // should take precedence over previous style
                        _applyStyle(style, prev);
                      },
                      newStyle,
                      parentsScopesToApply,
                    ),
                  );
                };
              };
            },
            None,
            p,
          );

        switch (result) {
        | None => f(scopeParents)
        | Some(result) =>
          let foreground =
            switch (result.foreground) {
            | Some(v) => v
            | None => default.foreground
            };

          let bold =
            switch (result.bold) {
            | Some(v) => v
            | None => default.bold
            };

          let italic =
            switch (result.italic) {
            | Some(v) => v
            | None => default.italic
            };

          let background =
            switch (result.background) {
            | Some(v) => v
            | None => default.background
            };

          {background, foreground, bold, italic};
        };
      };
    };
  };
  f(scopes);
};
