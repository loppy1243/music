# A music notation compiler
This program, written in the Racket language, compiles an ASCII representaion of music
notation (see `test.music`) to one of several backends. Current backends are
* `assembly`    -- Output the internal assembly used by the compiler.
* `x86-64`      -- Output an x86-64 executable. Currently requires the presence of `gcc` and a
                   certain program named `beep`.
* `interpreter` -- Interprets the input, rather than compiling. Currently requires the
                   presence of the program `sox`.

The program can be invoked as
```
./music.rkt [-o OUTFILE] --backend BACKEND INFILE
```
