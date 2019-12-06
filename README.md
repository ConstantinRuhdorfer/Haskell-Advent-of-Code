# adventOfCodeHaskell

This is a repo for solutions for the advent of code written in haskell.

At the moment this solves the first two puzzles.

## Usage

A basic project setup looks like this (assumes the haskell-stack toolchain):

```sh
git clone git@github.com:ConstantinRuhdorfer/Haskell-Advent-of-Code.git
cd Haskell-Advent-of-Code
stack install
./build.sh
```

After that you can compile and run the programm with:

```sh
./build.sh
```

or (but really the same):

```sh
stack build
stack exec adventOfCodeHaskell [all|day[1...X]]
```

## Project structure

The relevant folders are:

```
.
+-- app/                  Entry point
|   +-- Main.hs
+-- input/                Input
|   +-- dayX.txt          Official input for problem day X.
|   +-- testX.txt         Some test file for developement testing for day X.
|   +-- ...
+-- src/                  Problem solutions
|   +-- DayX.hs           Complete solution for porblem day X.
|   +-- ...
```

## Note

Every solution to a puzzle is intenionally pretty self-contained eventhought they could share more code.
This is to preserve readability for everyone who is interested in seeing the complete solution to one problem while being pretty unfamiliar with haskell or the structure of this project.

If I ever think that sharing code will improve readability I will do so.
