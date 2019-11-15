# Duet

A tiny language, a subset of Haskell aimed at aiding teachers teach Haskell

## Usage

This comes as a library and as a program capable of running Duet programs.

```
$ duet --help
Duet interpreter

Usage: duet [--version] [--help] COMMAND
  This is the interpreter for the Duet mini-Haskell educational language

Available options:
  --version                Show version
  --help                   Show this help text

Available commands:
  run                      Run the given program source
```

Example:

integers.hs:
```haskell
main = 3 + ((2 + -3) - 3)
```

Output for this program:

``` haskell
$ duet run examples/integers.hs
[1]
3 + ((2 + -3) - 3)
[2]
3 + (-1 - 3)
[3]
3 + -4
[4]
-1
```
