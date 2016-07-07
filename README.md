codingame-hs
============

A simple library to play with the Codingame site.

Usage
-----

For now, this library includes two modules:

-   **SourcePackager** to create monolithic sources from multiples files.
-   **WebServices** to gain access to the Codingame web services API from your code.

A typical usage is to combine these two modules to submit a complex bot spanning many modules to the
Codingame IDE (or directly to the arena) to test it against other players into a test session:

        main = do
            source <- createMonolithicSource "src/Golgoth.hs"
            credentials <- readCredentials "credentials.json"
            play credentials "Coders Strike Back" source [IdeCode, DefaultAi] Nothing

Install
-------

A <code>cabal install</code> will do the trick or just <code>cabal haddock</code> if you are only
interrested in checking out the documentation.
