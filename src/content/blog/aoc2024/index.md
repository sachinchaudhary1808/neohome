---
title: I used Haskell for Advent of Code 2024
slug: aoc2024
summary: |
    Some thoughts and cool things that I learn about this purely functional
    language, with the excuse of doing AoC.
pubDate: 2024-12-14T12:57:20Z
draft: false
---

This year I've been doing [Advent of Code](https://adventofcode.com) using
Haskell as the language of choice. For AoC, I usually only have the motivation
for doing the first week of problems, after that it becomes a chore for me.

Still, these 7 days were quite a learning experience of the language, and I
wanted to share some thoughts and things I learn, in no particular order. Let's
go!

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

## Day 4: Ascii maps and arrays

I'm kind of a hater of the exercises from AoC that involve ASCII maps, and this
is no less. For this day, you would have to find some patterns of text in the
map. Part 1 requires you to find the word "XMAS" in many directions
(horizontal, vertical, diagonal, reverse of all directions, etc), and part 2
requires you to find the pattern that shapes an "X" made of "MAS" (an X-MAS):

```
..M.S.
...A..
..M.S.
......
```

Part 1 is relatively easy, having to find the string "XMAS" along different
views of the same map (columns, rows, etc). For part 2, I decieded to use the
library `array`, which contains a generic `Array L T`, parametrized for a
location type and a content type. Our ASCII maps can be parsed into:

```haskell
import Data.Array.IArray as A
import Linear (V2 (V2))

type Map = A.Array (V2 Int) Char
```

Then, finding the pattern of X-MAS can be done by traversing all row and column
indices, and visiting the neighbours with the array acces operator:

```haskell
(!) :: Map -> (V2 Int) -> Char
```

I found it very neat to use the instance of `Monad` of lists, such that you
could get indices like this:

```haskell
day4part2 input = do
    let ainput = ...

    let res = do
            i <- [1 .. n - 2]
            j <- [1 .. m - 2]
            let center = ainput ! (i, j)

            let a = ainput ! (i - 1, j - 1)
            let b = ainput ! (i + 1, j + 1)
            let c = ainput ! (i - 1, j + 1)
            let d = ainput ! (i + 1, j - 1)

            return ((center == 'A') && isCross a b c d)
    
    print $ length (filter id res)
```

By this time, I found some other posts about people using Haskell for AoC, and
found about the `Comonad` class, implemented by `Store`. They used the
properties of `Comonad`, which are the following (simplified).

```haskell
class Monad m where
    return :: a -> m a
    (>>=) :: m a -> (a -> m b) -> m b

class Comonad w where
    extract :: w a -> a
    extend :: (w a -> b) -> w a -> w b
```

Similarly to `Monad`, but reverse, `extend` takes a function of a wrapped value
to a value (the opposite of bind), and `extract` is the opposite operation of
`return`.

Having a `Store` that implements `Comonads`, allows you to:

- Extract the central value of the Store (which can be sought).
- Apply a function that takes a `Store` of points to check if it is a cross, and
  then extend it to all possible values.

## Day 5: More parsing

This day involves parsing a list of sorting rules, and checking them on a list
of numbers. For example, the rule `13|45` would mean that 13 must always come
before 45. The input list would contain a list of rules, and then the list of
numbers.

I decided to make my parser generic enough, such that it could accept rules and
list of numbers in any order:

```haskell
myParse :: [String] -> ([Rule], [[Int]])
myParse [] = ([], [])
myParse (s : ss) = case (rule, list) of
    (Right r, _) -> first (r :) next
    (_, Right l) -> second (l :) next
    _ -> next
  where
    rule = runParser parseRule "" s
    list = runParser parseList "" s
    next = myParse ss
```

I think it's pretty neat this way of generating the final output of rules and
numbers, by using the `:` to map the result into either parts of the tuple --
tuples implement `Bifunctor`, which has `first` and `second`, the mapping
functions for either side.

The function is recursively defined, such that the result of the parsing is
appended to the result of parsing the next items.

## Day 6: Simulations

This day involves parsing yet another ASCII map, and running a simulation of an
arrow that travels it. It moves in one of the 4 main directions and can find a
boulder in its way, that makes it turn to the right -- very similar to one of
those ice puzzles in Pokemon.

I wrote a function that runs one step of the simulation, that can return a
`Maybe Position` that means that the arrow left the map.

```haskell
sim :: Map Char -> Position -> Maybe Position
```

Finding the exit point is just a matter of running the simulation function,
until we return a `Nothing`.

For part 2 it gets more interesting, as we are required to check in which
position of the map we can put a new boulder that would result in an infinite
loop. There are many ways to create optimized solutions for this task, by
storing previous paths from previous simulations, etc. However, I decided to
try to parallelize the problem, so that I could run many simulations at the same
time using all my CPU cores.

Parallelizing code for Haskell is not trivial for a new user. There are things
that you don't have to care about (memory lifetimes), but new things appear,
like the lazy evaluation model of the language. I've already read a book about
this, and I also recommend it to the reader: "Parallel and Concurrent
Programming in Haskell", by Simon Marlow. I wasn't an expert by any means after
reading it, but was mas first actual use-case for CPU-bound parallelization:

```haskell
    loops <-
        mapConcurrently
            ( \(i, map) -> do
                let !res = isLoop initialPos map
                putStrLn $ "Done: " <> show i
                return res
            )
            (zip [(0 :: Int) ..] charMaps)

    pPrint $ length (filter id loops)
```

I went with `mapConcurrently` from the `async` package (not the `parallel`
package), and seemed to parallelize somehow correctly.

It's nice to have a language with native support for green threads, and that you
don't have an explicit type for async actions (see `Future` in Rust). Any
Haskell code can be sent to multiple threads or ran concurrently thanks to its
runtime system.

## Day 7: Finale

The last day I did, involved check if a number could be composed by performing
either sum or multiplication in a string of numbers. My solution involved having
a recursively-defined function, that would "branch" like a tree until it found
the result:

```haskell
run :: Int -> Int -> [Int] -> Bool
run target acc [x] = (acc + x) == target || (acc * x) == target
run target acc (x : xs) = run target (acc + x) xs || run target (acc * x) xs
```

After this day, I was already feeling that the event was getting very repetitive
for me -- parse a file with megaparsec, traverse the input in some clever way,
rethink the traversal for part 2, repeat. In any case, I think it was a nice
learning experience, not only for what I mention in this post, but also for
tiny details in the general syntax or classes of the language.

See you next year in another language!

