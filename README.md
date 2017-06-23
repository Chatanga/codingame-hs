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
        playLatest credentials source [IdeCode, DefaultAi] Nothing
```

Known limitations
-----------------

**SourcePackager**

-   Although the module parses the sources provided to it to create a monolithic output, it won't be
    able to solve any name clash between functions, nor handle incompatible qualified and/or hidden
    import directives.

**WebServices**

-   The Codingame web API is not documented and keeps changing, regularly breaking this module.

-   The credentials system should only work for pure Codingame accounts as opposed to linked
    accounts using Google+ for instance.

Install
-------

The library now uses [Stack](https://docs.haskellstack.org). A such it is now `stack install`
instead of `cabal install` (or just `stack haddock` if you are only interrested in checking out the
documentation.).
