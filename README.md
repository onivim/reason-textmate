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

## Usage

```
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
let (scopeStack, tokens) = Tokenizer.tokenize(~lineNumber=1, ~scopeStack=Some(scopeStack), ~scopeName, "console.log('Hello, again!')");
```

## Contributing

Contributions are welcome!

New changes must:
- Add test coverage
- Pass all existing tests (`esy '@test' run)

## License

[MIT License](./LICENSE)

Copyright 2019 Outrun Labs, LLC
