# Editor

This is the editor for the language developed in Aufgabe 2. We are using a Haskell Stack project and GTK3+ as the GUI library. 

GTK is available through haskell-gi, a haskell binding library, which is required to use C libraries such as GTK.


## Installation

- Install GHCup (https://www.haskell.org/ghcup/) 
  - GHCup makes it easy to manage your entire Haskell development environment (GHC, stack, cabal, HLS)
- Using GHCup, install Stack and select the latest version (`2.11.1`)
- Install the requirements for haskell-gi (https://github.com/haskell-gi/haskell-gi)
- Run `stack run` in the cmdline to build and execute the application


## Development

I use IntelliJ with the IntelliJ-Haskell (https://plugins.jetbrains.com/plugin/8258-intellij-haskell) plugin, although you can use any IDE.

We should try to keep a modulized and maintainable code base, so split complex logic up into multiple small functions and modules.

### Project Structure

- `/app` holds the `Main.hs`, the entry point of the application
  - `Main.hs` should only contain the editor startup, not the actual UI components and logic, etc.
- `/src` contains the library modules, such as the `Highlighting`, `UIComponents`, `FileOperations`, etc.
- `package.yaml` describes the project settings and build dependencies

