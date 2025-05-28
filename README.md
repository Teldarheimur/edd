# Ð (edd) - a small language

It's a language.

## Usage

Linking and assembling require `tl` and `tasm` to be in the path.
Using the flag `-S` produces only the assembly and thus does not require them.

In addition, linking currently links to a library `std.savn` which
needs to have symbols for `panic` and various called library functions, and
should provide an entrypoint that references a symbol `main` in the source.

To compile just an object and skip linking, use the flag `-c`.
