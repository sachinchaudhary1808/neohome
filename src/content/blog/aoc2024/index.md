---
title: Advent of Code 2024 reflections
slug: aoc2024
summary: FIXME
pubDate: 2024-12-08T14:55:47Z
draft: true
---

This year I've been doing [Advent of Code](https://adventofcode.com) using
Haskell as the language of choice. For AoC, I usually only have the motivation
for doing the first week of problems, after that it becomes a chore for me.

Still, these 7 days were quite a learning experience of the language, and I
wanted to share some thoughts and things I learn, in no particular order. Let's
go!

## Day 0: Parsing command line arguments

Before getting into solving the problems, the day before December I just put in
place some boilerplate for running the different days. I didn't go with any
fancy AoC framework, but rather kept it simple.

All solutions are built into the same binary, and I select with solution to run
by passing some flags. The parsing of the flags is done with
`optparse-applicative`. I'm a big fan of parsing flags into data structures (see
`clap-derive` for Rust), and it is very simple at just what you need it to do.

First, you declare you datastructure into which you want to parse CLI flags on,
and then define your parser function, which is composed from many "mini-parsers"
that build up the struct. Unlike `clap-derive` from Rust, I'm not a fan of
having this spread into 2 places, but it's pretty neat regardless.


```haskell
data Cli = Cli
    { day :: Integer
    , input_file :: FilePath
    , part :: Integer
    }
    deriving (Show)

cli :: Parser Cli
cli =
    Cli
        <$> option
            auto
            (long "day" <> short 'd')
        <*> strOption (long "input-file" <> short 'i')
        <*> option auto (long "part" <> short 'p')

main :: IO ()
main = do
    parsedCli <- execParser $ info (cli <**> helper) fullDesc
    -- ...
```

Note that each flag produces a `Parser T`. For example, a `Parser Integer`.
Also, `Cli` is both the type name, and a constructor function with type `Cli ::
Integer -> FilePath -> Integer -> Cli`. If we imagine a simpler case, with only
the day, we would have `Cli :: Integer -> Cli`. As `Parser` is a `Functor`, we
can use `fmap` or `<$>` to lift our constructor over the parser, converting a
`Parser Integer` to a `Parser Cli`. Neat!

If we have a constructor with more than 1 argument though 
-- which is the case -- we use `<*>` from `Applicative` to thread our
partially-applied function through all the argument parsers.

## Day 3: Parsers!

This was the first day that required parsing ASCII numbers of some variable
lengths, so I finally decided to bite the bullet and go with a parsing library,
`megaparsec`. I had already used a similar library in Rust (`chumsky`), so the I
could mostly translate my knowledge from there.

You could think of a parser as a function that takes some input `I` and emits
some output `O`. The idea behind parser combinator libraries like `megaparsec`,
is that it provides many "mini-parsers". These parsers are not to
be used alone, but rather to be composed on top of each other. For example,
you could find a parser that takes some string as input, and tries to interpret
it as a number, otherwise fails.

```haskell
-- here, Parser T is a parser that takes strings
decimal :: Parser Integer

-- A parser that finds for some string
string :: String -> Parser String
```

You could use these parsers to create a new parser that tries to identify a
number in parentheses, by parsing and ignoring them:

```haskell
decimalInParens :: Parser Integer
decimalInParens = string "(" *> decimal <* string ")"

-- Or, abstracting it away to anything between parens:
inParens :: Parser a -> Parser a
inParens p = string "(" *> p <* string ")"
```

As you can imagine, you can make a library out of combinators like the one we
just wrote `inParens`. Some of them (imagine the meaning from the function name)
are `many`, `sepBy`, `skipMany`, etc.

The parsers from the library also implement `Monad`, so you can also use
do-notation for some "imperative-like" definitions:

```haskell
-- parse mul(3,2) as 6
someExpr :: Parser Integer
someExpr = do
    void (string "mul(")
    n1 <- decimal
    void (string ",")
    n2 <- decimal
    void (string ")")
    return (n1*n2)
```


## Nix + Haskell

As you might now from my numerous posts, I'm an avid [Nix](https://nixos.org)
user. It is a functional and reproducible package manager, that allows the user
to have user-level package management with "dev shells".

Usually, you use Nix to provide both the toolchain and packages (see C/C++
ecosystem), or you can just install the toolchain and it brings its own packages
(see Go). For Haskell, there are not only 2 toolchains available -- cabal and
stack -- but also it is possible to do the 2 styles of devshells -- bringing the
dependencies from Nix, or letting it do its thing.

It is also possible to thread your package through `pkgs.haskellPackages`, which
you might find in many snippets or tutorials. There's also the point with having
to match `haskell-language-server` with the same ABI as your GHC, or the fact
that some functions incur in "Import from Derivation", a Nix feature that can
have some weird consequences.

Through trial and error, I kinda have figured out how to do Haskell with Nix,
but I'm worried about what a beginner with Haskell would need to learn. Perhaps
the API surface for Nix+Haskell is too large, and we should need a simple
interface?

In any case, this is the shell I ended up using:

```nix
with import <nixpkgs> { };
{
  regular = mkShell {
    packages = [
      haskellPackages.cabal-install
      haskellPackages.haskell-language-server
      haskellPackages.fast-tags # for haskell-tools.nvim
      haskellPackages.threadscope
      aoc-cli
    ];
  };

  pkgShell = haskellPackages.developPackage {
    root = lib.cleanSource ./.;
    returnShellEnv = true;
  };
}
```
