## TODO

1. Solve "skipped token" problem
    - still cumbersome and unclear when to use
2. Incremental parsing
3. Preprocessor
    - defines?
4. LSP
5. Tests
6. Variable scopes, symbols
7. TToken.parent

### Variable scopes and symbols

The main usage is:

1. Errors like symbol is undefined - find a symbol by name in current scope. Implementation: hashmap by name.
2. Errors like symbol is of incorrect type - same as p.1 + check type against the allowed types.
3. Return an information about a symbol under cursor:
    - get token from cursor position
    - get symbol from TIdentifier
4. Find all references of a symbol under cursor - same as p.3 + store full list of references in the symbol.

Objects???

### Incremental parsing

Typical approach seems to be finding the common root that is affected and re-parsing it.

Or, alternatively, caching the AST nodes that are context independent.
For starters, context independent are primitives:
- reserved words
- identifiers
- numbers

So basically lexical items. But additionally, some other items can be context independent...

At the very least, we can skip parsing the nodes that come before the diff.

What needs to be done:
- update contents
- update positions of tokens e.g. `if pos > diffEnd then inc(pos, lenDiff)`
- determine the biggest node that spans across diff start
- remove all nodes starting from it, save token.start
- set the cursor to the token.start and start parsing until we reach the first token that was not affected
- replace affected nodes with the newly parsed nodes

We need a mechanism for caching tokens based on their position.

### Skipped tokens 2

Another idea,

```pascal
    ifToken := TReservedWord.Create(ctx, rwIf);
    ctx.RemoveAnchor(rwIf);

    ctx.AddAnchor(rwThen);
    ctx.AddAnchorExpression;
    ctx.AddAnchorStatement;

    ctx.SkipUntilAnchor;

    condition := TExpression.Create(ctx);
    ctx.RemoveAnchorExpression;

    ctx.SkipUntilAnchor;

    thenToken := TReservedWord.Create(ctx, rwThen);
    ctx.RemoveAnchor(rwThen);

    ctx.SkipUntilAnchor;

    statement := TStatement.Create(ctx);
    ctx.RemoveAnchorStatement;
```

Where:

`AddAnchor` => `inc(tokensToSkip[tokenIndex])`;
`RemoveAnchor` => `dec(tokensToSkip[tokenIndex])`.

`SkipUntilAnchor` => `SkipTrivia; while (tokensToSkip[Cursor[0]] > 0) ParsePrimitive;`


`DeterminePrimitiveType` => ' => string
                            a..zA..Z_ => ident or reserved word
                            0..9 => number
                            +-( => expression
                            ; => statement

Primitive = string | number | ident | reserved word | trivia
