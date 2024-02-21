# Polish
An interpretor of a simple language using Polish notation

## Prerequisite
- OCaml
- [Dune](https://github.com/ocaml/dune)
- [Zarith](https://github.com/ocaml/Zarith)

## Features
- Code Evaluation
- Code Simplification
- Uninitialized Variable Analysis
- Integers' Sign Analysis
  
## How to Use
- Write a program in Polish
- Compile the program by typing `make`, a `polish.exe` file will be created.
- Execute run
  ```
  ./run [mode] [path_to_file]
  ```
with mode being one of these options :
- `--eval`
- `--simpl`
- `--vars`
- `--sign`
