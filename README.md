# simple-text-format

This library provides a very simple format string syntax with named identifiers based on `text` and `attoparsec`.

Syntax for identifiers is `${variable-name}`.
Please note that it is whitespace sentitive, meaning `${var]` references the variable `"var"` whereas `${ var}` referenced the varaible `" var"`.
The rendering is agnostic to the data structure you use to keep the identifiers.
The formatting function expects simply a function `Text -> Maybe Text`
There is currently no escaping mechanism, meaning `$` parses to `"$"` but there is no way to get a literal `${`.

```hs
let formatStr = "A string with ${var} and ${var2}"
let identMap = [("var", "something"), ("var2", "something else")] :: HashMap Text Text
format' formatStr (lookup identMap)
-- A string with something and something else
```
