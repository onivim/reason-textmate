# reason-textmate
ReasonML native library for working with TextMate grammars

## Building

- `esy install`
- `esy build`

### Tests

- `esy '@test' install`
- `esy '@test' run`

### Benchmarks

- `esy '@bench' install`
- `esy '@bench' run`

### Documentation

Latest docs are available here: https://onivim.github.io/reason-textmate/textmate/index.html

- `esy '@docs' install`
- `esy '@docs' build`
- `esy '@docs' update`

## Performance

| Benchmark | `reason-textmate` | `vscode-textmate` | % diff |
| --- | --- | --- | --- |
| jQuery 2.0.3 | 543ms | 618ms | `reason-textmate` is ~12% faster |
| bootstrap.css | 71 ms | 245 ms | `reason-textmate` is ~71% faster |

Benchmarks were averaged across 3 trials on my Windows 10 dev machine. It's surprising that the CSS is so much faster; it is possible there is a bug, although I compared the highlight output for `bootstrap.css` between VSCode and Onivim 2 using this library, and did not see differences.

The main bottleneck for performance is running the Oniguruma regular expressions (`onig_search`) - anything that can be done to reduce the number of times we need to run a search can greatly improve performance. There are some other optimizations that could be made, for example, we're not using the [flambda optimizing compiler](https://caml.inria.fr/pub/docs/manual-ocaml/flambda.html) today - and we're doing some wasteful list manipulation and string manipulation for scopes. But this is pretty low overhead compared to the main bottleneck of evaluating the regular expression.

## Roadmap

Most of the textmate grammar syntax is supported, but there are a few missing features we need for full parity:

- While conditions
- Nested patterns in capture groups

> NOTE: These features are not used by the grammars in the benchmarks

## Usage

```reason
open Textmate;

// Create a grammar repository
let grammarRepository = GrammarRepository.ofFilePath("source.js", "/path/to/js-grammar.json");

// Create a tokenizer
let tokenizer = Tokenizer.create(grammarRepository);

// Tokenize a line. Tokenizing returns a scope stack and a set of tokens.
let (scopeStack, tokens) = Tokenizer.tokenize(~lineNumber=0, ~scopeStack=None, ~scope="source.js", tokenizer, "console.log('Hello, world!')");

// Print tokens:
List.iter((token) => print_endline("Token: " ++ Token.show(token), tokens);


// Tokenize a second line, using the scope stack from the previous line.
let (scopeStack, tokens) = Tokenizer.tokenize(~lineNumber=1, ~scopeStack=Some(scopeStack), ~scopeName, tokenizer, "console.log('Hello, again!')");
```

## Contributing

Contributions are welcome! We'd :heart: help implementing the remainder of functionality.

New changes must:
- Add test coverage
- Pass all existing tests (`esy '@test' run)

## License

[MIT License](./LICENSE)

Copyright 2019 Outrun Labs, LLC
