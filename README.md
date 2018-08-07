codingame-hs
============

A simple package to play with the [Codingame](https://www.codingame.com/) site.

Usage
-----

For now, this package includes two modules:

-   **SourcePackager** to create monolithic sources from multiples files.
-   **WebServices** to gain access to the Codingame web services API from your code.

A typical usage is to combine these two modules to submit a complex bot spanning many modules to the
Codingame IDE (or directly to the arena) to test it against other players into a test session:

```haskell
main = do
        source <- createMonolithicSource "src/Player.hs"
        credentials <- readCredentials "credentials.json"

        -- To play a specific past challenge (using its name, not its ID).
        playInIDE credentials (ChallengeTitle "Coders Strike Back") source [IdeCode, DefaultAi] Nothing

        -- To play any ongoing challenge (if any).
        playInIDE credentials OngoingChallenge source [IdeCode, DefaultAi, DefaultAi, DefaultAi] Nothing

        -- To submit a code into the arena.
        submitToArena credentials OngoingChallenge source

        return ()
```

If you are looking for a more detailled usage, have a look to this
[skeleton project](https://github.com/Chatanga/codingame-hs-skeleton) which demonstrate how this package
could be used in practice during a contest. Among other things, it introduces a small trick to easily
replay locally a game, conveniently allowing the use of GHCI to debug the code.

Known limitations
-----------------

**SourcePackager**

-   Although the module parses the sources provided to it to create a monolithic output, it won't be
    able to solve any name clash between functions, nor handle incompatible qualified and/or hidden
    import directives.

**WebServices**

-   Changes in the Codingame web API often break this module, even when they shouldn’t matter.

-   The credentials system should only work for pure Codingame accounts as opposed to linked
    accounts using Google+ for instance.

Install
-------

The package now uses [Stack](https://docs.haskellstack.org/), so nothing to add.

Since this package is not on [Hackage](https://hackage.haskell.org/),
the simplest way to actually use it for one of your Codingame bots (that’s how I proceed for mines)
is to create a dedicated Stack project for you contest bot,
download the codingame-hs project alongside,
then add the following depenpency into your `stack.yaml` file:

```yaml
packages:
- .
- ../codingame-hs
```

A `stack build` will (re)build your project as well as the package.
