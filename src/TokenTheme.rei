/*
 TokenTheme.rei

 Interface for textmate theme matching
 */

open ThemeScopes;

/*
   [themeSelector] describes a [string] selector,
   along with the [TokenStyle.t] styling associated with that selector.
 */
type themeSelector = (string, TokenStyle.t);

/*
   [t] is a description of a TextMate theme
 */
type t;

/*
   [create] builds a TokenTheme [t] from a list of styles
 */
let create:
  (
    ~defaultBackground: string,
    ~defaultForeground: string,
    list(themeSelector)
  ) =>
  t;

/*
    [of_yojson] instantiates a TokenTheme [t] from JSON
 */
let of_yojson:
  (~defaultBackground: string, ~defaultForeground: string, Yojson.Safe.t) => t;

/*
   [empty] is an empty TokenTheme [t] with no selectors
 */
let empty: t;

let union: (~defaultBackground: string, ~defaultForeground: string, t, t) => t;

/*
    [match] returns the resolved style information,
    given the scopes [string]. The [scopes] should include
    the full ancestor list, separated by spaces, for example:
    "text.html.basic source.php string.quoted.double.php"

    Returns styling information based on the selecotrs.
 */
let match: (t, string) => ResolvedStyle.t;

/*
   [show] returns a string representation of the TokenTheme [t]
 */
let show: t => string;
