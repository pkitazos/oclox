# Scanning

1. The lexical grammars of Python and Haskell are not regular. What does that mean, and why aren’t they?

I'm assuming it has to do with their similarity, which is that they use whitespaces and newlines for nesting, scoping, etc. which means they have more than one non-terminal symbol (the space character and/or the newline character) separating different expressions?

2. Aside from separating tokens—distinguishing `print foo` from `printfoo`—spaces aren’t used for much in most languages. However, in a couple of dark corners, a space does affect how code is parsed in CoffeeScript, Ruby, and the C preprocessor. Where and what effect does it have in each of those languages?

Not sure about CoffeeScript, but in Ruby the difference is in function calls:

```
foo(1)
```

is equivalent to:

```
foo 1
```

but not equivalent to:

```
foo (1)
```

since the third form passes a parenthesised expression to foo, the equivalent of:

```
foo((1))
```

In this example there's probably no difference, but it's likely that there's a big difference for multi-arg functions

In C it's to do with macro definitions:

```
#define foo(x) ...;
```

is not the same as:

```
#define foo (x) ...;
```

because the pre-processor stops parsing the macro name at the first whitespace, so it doesn't parse it as requiring an arg x, but rather that the expression `(x)` is the first expression in the macro body.

3. Our scanner here, like most, discards comments and whitespace since those aren’t needed by the parser. Why might you want to write a scanner that does not discard those? What would it be useful for?

For a formatter. You need the parser to allow the source to survive a round trip, so you need trivia tokens if you wish to write a formatter that doesn't remove comments.
