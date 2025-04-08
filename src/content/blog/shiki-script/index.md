---
title: Script+Shiki for beautiful terminal snippets
pubDate: 2025-04-08
slug: shiki-script
draft: false
summary: A guide to converting terminal ANSI escape sequences into colorful, syntax-highlighted snippets using Shiki.
---

Today I learn how you can keep the colors of the output of your terminal applications, when pasting
then into your code blocks in Markdown. This is not about syntax highlight for a language like Python,
but rather about the output of terminal applications.

You will go from this:

```
$ npm exec astro check
16:03:12 [content] Syncing content
16:03:12 [content] Synced content
16:03:12 [types] Generated 177ms
16:03:12 [check] Getting diagnostics for Astro files in /var/home/ayats/Documents/neohome...
Result (39 files):
- 0 errors
- 0 warnings
- 0 hints
```

To this:

```ansi
$ npm exec astro check
[1G[0K[2m16:06:02[22m [34m[content][39m Syncing content
[2m16:06:02[22m [34m[content][39m Synced content
[2m16:06:02[22m [34m[types][39m Generated [2m183ms[22m
[2m16:06:02[22m [34m[check][39m Getting diagnostics for Astro files in /var/home/ayats/Documents/neohome...
[1mResult (39 files): [22m
[2m-[22m [1m[31m0 errors[39m[22m
[2m-[22m [1m[33m0 warnings[39m[22m
[2m-[22m [2m0 hints
```

## ANSI control sequences

Terminal colors are generated with escape sequences. Simply put, if you (a program) wants to make
some text <span class="text-red-500">red</span>, you have to send a special sequence of characters to the terminal
to indicate that the following text is red. This sequence is interpreted by the terminal, and removed from the actual text.

For the terminal, every sequence starts with the 28th ascii character, that is a `char` with value `27` (`0x1B` hex) which is commonly
known as `ESC`. This character doesn't have a represntation, so if I paste it here you will see some fallback character: ``.
You've probably seen it written in other ways, like `\x1b` or `\e`. These are easier shorthands that use regular ASCII characters to
represent the ``, which is cumbersome to write and not printable.

Following it, you will find the character `[`, some function arguments and the function to call. You can find a longer explanation here: https://notes.burke.libbey.me/ansi-escape-codes. For color, we care about the `m` instruction.

Knowing this, to set the foreground color we simply send the instruction to the terminal and then reset it:

```
[31m(This is some red text)[0m
```

```ansi
$ printf "\e[31m(This is some red text)\e[0m"
[31m(This is some red text)[0m
```

The important bit is that **we cant to preserve this information**. There are many tools to do this, but the one I've found is `script`.

- Run `script`.
- Inside the shell, you any commands you want colored output of.
- Exit `script`.
- You will have a `typescript` file with all the raw ANSI escape sequences for the command logs.

```ansi
$ script
Script started, output log file is 'typescript'.

$ printf "\e[31mHello\e[0m"
[31mHello[0m
$ exit
```

## ShikiJS

Now that we have a `typescript` file with all our raw escape sequences that mark color, we need to give it to a syntax highlighter that supports it.

I'm using the highlighter [Shiki](https://shiki.matsu.io/), which is the default for [Astro](https://astro.dev), and other projects like
[Slidev](https://sli.dev). Discord's syntax highlight has support for ANSI.

Simply, mark your code blocks with the `ansi` language, and paste your raw escape sequences:


~~~
```ansi
$ printf "\e[31mHello\e[0m"
[31mHello[0m
```
~~~

Will be rendered as:

```ansi
$ printf "\e[31mHello\e[0m"
[31mHello[0m
```

## Closing thoughts

That's all! I hope this helped you beautify your own blog or documentation site!
