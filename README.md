# simp-cal

_Simple Calculate_

This project parses a calculation into expressions, and then evaluates it.

- Probably fast,
- Maybe efficient,
- But memory safe (_Rust_ #1)

## How to use

Calculating simply can be implemented like this:

```rust
let cal = "1+1"
let eval = TokenStream::from_text(cal).evaluate()?;

assert_eq!(eval, 2.0);
```

The `TokenStream` can be parsed back into text by using `as_text`;
note: this doesn't return any errors, so use `is_valid` to check beforehand.

```rust
let cal = "1+-(4*2.5)";
let stream = ExprStream::from_text(cal)?;

// Returning None, means it's valid
if stream.is_valid().is_none() { 
  /// The boolean parameter means enable spacing between infix operators (e.g. + or *)
  /// If it were true, `1 + -(4 * 2.5)`
  let reconstructed = stream.as_text(false);
  assert_eq!(reconstructed, cal);
}
```

## Formatting Rules

Along with all the normal formatting rules of most calculates, like the order of operations, these are some syntax to be aware of:

Two unary operators can not be next to each other without the use of brackets (this includes suffix and prefix combinations).
Doesn't work: `--1`, `-2!`
Fixes: `-(-1)`, `-(2!)`

The order of exponents do not from _up to down_ (instead goes by _down to up_).
Doesn't work as expected: `2^3^4`
Instead: `2^(3^4)`

## TODOs

- [x] tokenization
- [x] expression parsing
- [x] evaluation
- [x] brackets
- [x] negative numbers
- [x] 100% test coverage
- [ ] substitution
- [ ] functions

<sub>Made by blockitifluy, no dependencies</sub>

# Example Program

The example program can be found in the `example` directory.

## Options

- `-v`, `--version`: Prints the program's version,
- `-h`, `--help`: Show the help message,
- `-V`, `--verbose`: Show the tokens and expressions being parsed,
- `--no-eval`: Doesn't evaluate the equations, if the `--verbose` option isn't chosen, then just prints out the expressions in the calculations.
