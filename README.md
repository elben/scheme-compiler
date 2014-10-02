# Incremental Scheme Compiler

Scheme compiler based on Abdulaziz Ghuloum's papers (see references below).

This is a toy; not to be used for production.

### Overview

This project is a Scheme program that compiles our Incremental Scheme into x86.
The emitted x86 must be assembled together with `runtime.c`, a program that
translates x86 values back into Scheme values for printing.

The bulk of the compiler is in `src/compiler.scm`.

To make it easier to follow along, git logs contain the text `[ch.section]` to
represent the chapter and section from the tutorial (below). For example,
a git log with `[1.2]` implies this commit solved a problem presented in Chapter
1.2. The paper, however, follows different chapter/section numbering. Paper
sections are denoted by double brackets, like `[[3.2]]`.

The binary produced by the compiler runs only on Linux. Tested on Ubuntu.

# Using

When in the Petite REPL, you can see the X86 output, given some Incremental Scheme:

```
(compile-program '(fxadd1 3))
;     .text
;     .globl scheme_entry
;     .type scheme_entry, @function
; scheme_entry:
;     movl $12, %eax
;     addl $4, %eax
;     ret
```

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
petite run.scm
(test-all)
```

To check for warnings in the runtime:

```
gcc -Wall -fsyntax-only ctest.c runtime.c
```

# Addendum and Notes

Changes and notes from Abdulaziz Ghuloum's papers.

Papers refer to `emit-program`, but the tests require you to name your compiling
function `compile-program`.

# References

## Incremental Scheme Compiler

[An Incremental Approach to Compiler Construction](https://github.com/elben/scheme-compiler/blob/master/archive/incremental-scheme-compiler-paper.pdf)

[Compilers: Backend to Frontend and Back to Front Again](https://github.com/elben/scheme-compiler/blob/master/archive/incremental-scheme-compiler-tutorial.pdf)

[Nada Amin's implementaion](https://github.com/namin/inc)

[LtU: An Incremental Approach to Compiler Construction](http://lambda-the-ultimate.org/node/1752)

## Development References

[Scheme documentation index](http://scheme.com/csug8/csug_1.html#./csug:h0)

[x86 Assembly Instructions](http://en.wikibooks.org/wiki/X86_Assembly/X86_Instructions)

[Intel® 64 and IA-32 Architectures Software Developer Manuals](http://www.intel.com/content/www/us/en/processors/architectures-software-developer-manuals.html)

