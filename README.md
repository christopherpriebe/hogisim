# Hogisim
## Description
Hogisim is a combinational digital logic sandbox where users can construct digital circuits using the seven fundamental logic gates: NOT, OR, AND, XOR, NOR, NAND, XNOR.
Upon opening Hogisim, there are three options. The first option creates an empty sandbox, the second option loads a sandbox from a file, and the third option exits the program.
The board can be populated with multiple instances of fundamental logic gates, an input node which can be toggled to either 0 or 1, or an output node that is updated at execution time.
This program was written as a final project for CSE 230: Principles of Programming Languages taught at UCSD during the Fall 2022 quarter by Professor Nadia Polikarpova.

> **Archive notice:** this repository is preserved as-is for reference. It targets GHC 9.2.8 and `brick` 0.x and is not maintained against current versions of those libraries.

## Building

The project builds with **GHC 9.2.8**. Install via [`ghcup`](https://www.haskell.org/ghcup/):

```sh
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
```

After the installer finishes, make sure `~/.ghcup/env` is sourced in your shell, then:

```sh
ghcup install ghc 9.2.8 && ghcup set ghc 9.2.8
ghcup install cabal recommended
```

### With cabal

A `cabal.project` is included that pins the Hackage index to a date when the project's dependencies (notably `brick` < 1.0) were still current, so the build resolves reproducibly:

```sh
cabal update
cabal build
cabal run hogisim
cabal test
```

### With stack

A `stack.yaml` pinned to `lts-18.13` (GHC 8.10.7) is also provided. Note: on Apple Silicon this resolver requires LLVM 9–12 to be installed, which is no longer easily available via Homebrew. The `cabal` route above is recommended.

```sh
stack build
stack exec hogisim
stack test
```

## Known Issues
- Output nodes cannot be placed adjacent to each other due to the way in which the back-end searches the matrix
