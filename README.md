> Guess the category!

[![Check](https://github.com/lewis-weinberger/guess-the-category/actions/workflows/ci.yml/badge.svg)](https://github.com/lewis-weinberger/guess-the-category/actions/workflows/ci.yml)

A simple guessing/deception game inspired by
[Spyfall](https://spyfall.adrianocola.com/), using Discord
as the user interface.

## Usage

The bot is written in [Haskell](https://www.haskell.org/) using
[discord-haskell](https://hackage.haskell.org/package/discord-haskell).
It requires [GHC](https://www.haskell.org/ghc/) and
[cabal](https://www.haskell.org/cabal/):

```sh
git clone https://github.com/lewis-weinberger/guess-the-category
cd guess-the-category
cabal build
export DISCORD_TOKEN=<your-token-here>
export DISCORD_GUILD=<your-guild-here>
cabal run
```

The bot registers the following slash commands in Discord:

- `join`: join the lobby to play the next round
- `leave`: leave the lobby
- `set`: set the categories for the next round
- `new`: start a new round
- `reveal`: show the player their assignment (using an "ephemeral"
message)
