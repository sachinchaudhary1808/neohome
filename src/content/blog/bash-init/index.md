---
title: PSA about Bash init files
slug: bash-init
pubDate: 2025-03-22T10:50:11Z
draft: true
summary: |
    Overview of which files are loaded by bash depending on its
    different invocations
---

In this article I want to talk about which files are loaded by Bash.
This is a question I often run into, so not only I want to document it for
other people, but also for my future self.

## Cheatsheet

If you are in a hurry, this is a summary of the article.

| Login shell   | Non-login shell |
| --- | --- |
| `/etc/profile`| `/etc/bashrc` |

If you want to understand the reasons, keep reading.

## What is a shell

This question might seem obvious, but the answer is more nuanced that what you might think.

First, a shell is **a program you execute**. This is the least common denominator.

A shell can be defined at `/etc/shells`. This is a file containing one line for each valid shell
that can be assigned to a user with the `chsh`. This information is written into `/etc/passwd`.
If you try to `chsh` into a shell that is not listed, the program will error out.

```
# /etc/shells: valid login shells
/bin/sh
/usr/bin/sh
/bin/bash
# ...
```

```
$ grep root /etc/passwd
root:x:0:0:root:/root:/bin/bash
```

The shell is executed when a user logs in, for example when opening a terminal or using SSH. The shell
assigned to your user in `/etc/passwd` will be executed (more on that later).

Finally, shells usually are able to execute some scripting language, and the "main" one is POSIX shell
command language. This is what you usually know as a "shell script", and it might look like this:

```bash
#!/bin/sh

if [ -f file ]; then
  echo "File already exists"
  exit 1
else
  touch file
fi
```

While this language might seem universal, there's actually some variants of it:

- POSIX Shell language, aka "the original"
- Bash language, basically POSIX + extensions called [bashisms](https://mywiki.wooledge.org/Bashism)
- ZSH language, which also looks similar at a first glance, but [has differences](https://zsh.sourceforge.io/FAQ/zshfaq02.html).

Both Bash and ZSH are capable of running in a "POSIX-Compatible" mode. This means, they swap to interpreting
the script as a different language. `/bin/sh` is the de-facto way to run a POSIX sh script. In most Linux distributions,
`/bin/sh` is a symlink to Bash, which runs under the POSIX interpreter when called from the symlink, insted of enabling
the Bash language features (bashisms).

Finally, I want to address the elephant in the room: a shell is a very ambiguous term, and it can be really any
program. Yes, some shells can interpret POSIX sh scripts, but Bash and ZSH have their own language extensions. We
quickly find some outliers: `/bin/nologin` is also present in `/etc/shells`, and it is a program that does
nothing -- this is used to "trap" system accounts from actually login-in. We also have the Fish shell, is similar in
functionality to Bash and ZSH, has a different language from POSIX sh (as Bash and ZSH), but does not have a compatibility
layer (whether it is something you actually need is up for debate).

So, could you set `/bin/python3` as your login shell? I guess so.


## Login and interactive shells

## Argv hack

