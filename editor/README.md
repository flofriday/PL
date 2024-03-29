# Editor

![Screenshot](screenshot.png)

[Assignment3](https://tuwel.tuwien.ac.at/pluginfile.php/3542105/mod_folder/content/0/aufgabe3.pdf)

This is the editor for the language developed in Exercise 2. We are using a Haskell Stack project and GTK3+ as the GUI library. 

GTK is available through haskell-gi, a haskell binding library, which is required to use C libraries such as GTK.


## Installation

- Install GHCup (https://www.haskell.org/ghcup/) 
  - GHCup makes it easy to manage your entire Haskell development environment (GHC, stack, cabal, HLS)
- Using GHCup (with `ghcup tui`), install Stack and select the latest version (`2.11.1`)
- Install the requirements for haskell-gi (https://github.com/haskell-gi/haskell-gi)
- Run `stack run` in the cmdline to build and execute the application


## Development

We used IntelliJ with the IntelliJ-Haskell (https://plugins.jetbrains.com/plugin/8258-intellij-haskell) plugin, although you can use any IDE.

### Project Structure

- `/app` holds the `Main.hs`, the entry point of the application
  - `Main.hs` should only contain the editor startup, not the actual UI components and logic, etc.
- `/src` contains the library modules, such as the `Highlighting`, `UIComponents`, `FileOperations`, etc.
- `package.yaml` describes the project settings and build dependencies

