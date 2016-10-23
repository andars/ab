# ab: andrew's B compiler

```
11/3/71                                                                   B (I)

NAME        B -- language

SYNOPSIS    sh rc /usr/b/rc name

DESCRIPTION B is a language suitable for system programming. It is described
            in a separate publication B reference manual.


            The canned shell sequence in /usr/b/rc will compile the program
            name.b into the executable file a.out. It involves running the B
            compiler, the B assembler, the assembler and the link editor. The
            process leaves the files name.i and name.s in the current directory.

FILES   name.b, name.i, name.s.

SEE_ALSO    /etc/bc, /etc/ba, /etc/brtl, /etc/brt2, /etc/bilib,/etc/libb.a,
            B reference manual.
DIAGNOSTICS see B reference manual

BUGS    There should be a B command.

OWNER   ken, dmr
```
