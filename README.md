% LLBC

This is an implementation of the lambda lambda-bar calculus (or llbc for short), written in ocaml.

## Requirements:

This program requires the ocaml language (tested with version 3.11.2), see http://caml.inria.fr/ocaml/index.en.html.


## Installation:

Use the following command to build the program:

    make

The executable, called "llbc", can then be used on one of the provided example files:

    ./llbc -i examples/example_1

This produces a source file "output.tex", which can then be compiled with latex. The provided file "llbc_style.sty" is necessary for this compilation.  Alternatively, the following command displays the output on the terminal:

    ./llbc -i examples/example_1 -t

If no input file is provided with the "-i" option, the program reads on its standard input. The other options are as follow:

    -s N   : Only compute N reduction steps. The default is 32.
    -c     : Call latex to compile the produced file output.tex.


## Syntax:

The syntax used in the terminal output is valid, and can be copied and pasted back to the executable (the leading "->" or "=", when present, should then be omitted). This syntax uses unicode approximations of the symbols used in the latex output, such as 'ƛ' for the lambda-bar. For easier keyboard input, the following sequence of characters are recognized:

Symbol name:    keyboard        terminal output (accepted as input format)
Positives:
epsilon         ep              ε               
lambda          lp              λ               
plus            pp              +
merge           mp              <
connexion       cp              V
mu              mu              ν

Negative:
epsilon-bar     en              ὲ
lambda-bar      ln              ƛ
plus            pn              +                
merge           mn              <
backtrack       BT		        BT


