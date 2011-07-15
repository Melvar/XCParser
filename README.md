The `XCParser` module contains a parser (`file`) for .XCompose files and types
for the representation.

The `Extended` module contains the same, with extensions to the syntax (and
correspondingly to the representation types). Currently, there are two and a
half extensions:
- Grouping with `{}` allows common prefixes to be factored out.
- Names can be defined with `[name]` in place of a sequence and referenced by
  `@name` as a target
- Sequences can be empty

