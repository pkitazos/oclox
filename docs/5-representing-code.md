# Representing Code

## Challenges

1. Produce a grammar that matches the same language but does not use any of the notational sugar.

```
expr  -> expr ( "(" ( expr ( "," expr )* )? ")" | "." IDENTIFIER)+
      | IDENTIFIER
      | NUMBER
```
