codingame-hs
============

A simple library to play with the [Codingame](https://www.codingame.com) site.

Usage
-----

For now, this library includes two modules:

-   **SourcePackager** to create monolithic sources from multiples files.
-   **WebServices** to gain access to the Codingame web services API from your code.

A typical usage is to combine these two modules to submit a complex bot spanning many modules to the
Codingame IDE (or directly to the arena) to test it against other players into a test session:

```haskell
main = do
        source <- createMonolithicSource "src/Golgoth.hs"
        credentials <- readCredentials "credentials.json"
        -- To play a specific past challenge (using its name, not its ID).
        play credentials "Coders Strike Back" source [IdeCode, DefaultAi] Nothing
        -- To play any ongoing challenge (if any).
        playLatest source [IdeCode, DefaultAi] Nothing
```

Install
-------

A `cabal install` will do the trick or just `cabal haddock` if you are only
interrested in checking out the documentation.
