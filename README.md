# Incremental Scheme Compiler

Scheme compiler based on Abdulaziz Ghuloum's papers (see references below).

The binary produced by the compiler runs only on Linux. Tested on Ubuntu.

Under development—REPL not yet available.

### Overview

`compiler.scm` is a Scheme program that compiles our Incremental Scheme into x86.
This compiled form must be assembled together with `runtime.c`, which helps us
translate x86 values back into Scheme values.

To make it easier to follow along, git logs contain the text `[ch.section]` to
represent the chapter and section from the tutorial (below). For example,
a git log with `[1.2]` implies this commit solved a problem presented in Chapter
1.2. The paper, however, follows different chapter/section numbering. Paper
sections are denoted by double brackets, like `[[3.2]]`.

# Development

Download [Petite Chez Scheme](http://www.scheme.com/download/index.html#sec:petitechezscheme) (e.g. 64-bit threaded).

```
tar xvf pcsv8.4-ta6le.tar.gz
cd csv8.4/custom
./configure
make && sudo make install
```

Running tests:

```
cd ~/code/incrementing-scheme-compiler
petite compiler.scm
```

```
(test-all)
```

[Scheme documentation index](http://scheme.com/csug8/csug_1.html#./csug:h0)

# Addendum and Notes

Changes and notes from Abdulaziz Ghuloum's papers.

Papers refer to `emit-program`, but the tests require you to name your compiling
function `compile-program`.

# References

[Nada Amin's implementaion](https://github.com/namin/inc)

An Incremental Approach to Compiler Construction (see /archive/)

Compilers: Backend to Frontend and Back to Front Again (see /archive/)

http://lambda-the-ultimate.org/node/1752
