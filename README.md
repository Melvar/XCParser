The `XCParser` module contains a parser (`file`) for .XCompose files and types
for the representation.

The `Extended` module contains the same, with extensions to the syntax (and
correspondingly to the representation types). Currently, there are three and a
half extensions:

-   Grouping with `{}` allows common prefixes to be factored out.
-   Names can be defined with `[name]` in place of a sequence and referenced by
    `@name` as a target.
-   Sequence elements can be string literals to indicate the keysyms
    corresponding to the individual characters of the string.
-   Sequences can be empty.

Additionally, `Extended` contains a function `standardize` to convert it into
the representation of a standard .XCompose file, using the types from
`XCParser`.

