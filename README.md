# Haskell-Advent-of-Code

This is a repo for solutions for the advent of code written in haskell.

At the moment this solves the first two puzzles.

The current output of `all` is:

```sh
$ stack exec adventOfCodeHaskell all
"Calculating fuel for input file input/day1.txt"
5084676
"Executing programm in input/day2.txt with 12 and 2"
12490719
"Searching for a noun and a verb that together with the program in input/day2.txt will result in 19690720"
[20,3]
"Finding the distance of the intersection with the closest distance to (0,0):"
5672
"Finding the ammount of fitting codes between 240920 and 789857:"
750
```

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
stack exec adventOfCodeHaskell [all|day[1|...|X]]
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
|   +-- DayX.hs           Complete solution for problem day X.
|   +-- ...
```

## Note

Every solution to a puzzle is intenionally pretty self-contained eventhought they could share more code.
This is to preserve readability for everyone who is interested in seeing the complete solution to one problem while being pretty unfamiliar with haskell or the structure of this project.

If I ever think that sharing code will improve readability I will do so.
