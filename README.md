# fibonacci

This short Haskell module contains an example of using a State monad to compute a Fibonacci sequence.

We recursively call the State monad, but it doesn't grow the stack. It tracks the current multiplicand and accumulated result. In other words `State Int Int` operates on an accumulated result and a current input value.

Refer to 
+ [Stackoverflow post -- State Monad calculates a Fibonnaci Series](https://stackoverflow.com/questions/26901206/implementing-factorial-and-fibonacci-using-state-monad-as-a-learning-exercise)
+ [Tutorial on State Monad](https://mvanier.livejournal.com/5846.html)

The working data type is the huge `Integer` in lieu of the narrower `Int`. We can produce a profoundly large sequence. 
We've tested with `fibonnaci 500000`, resulting in a pages-long numeric result within seconds 
(on a current MacBook pro). 

## Module

`Main.hs`

Complete source:

```haskell

module Main where

import           Control.Monad.State.Lazy

fibArg :: Integer
fibArg = 1000

main :: IO ()
main =
  do
    putStr "fibonacci of "
    print fibArg
    print (fibonacci fibArg)

-- A pair of functions that carry out the fibonacci algorithm.
-- Invocation e.g.: fibonacci 1000

fibsState :: State (Integer, Integer, Integer) Integer
fibsState =
  get >>= \(x1, x2, n) ->
    if n == 0
      then return x1
      else put (x2, x1 + x2, n - 1) >> fibsState

fibonacci :: Integer -> Integer
fibonacci n = evalState fibsState (0, 1, n)

```

## Development Environment

+ Haskell "Stack" package (It can create new Stack projects or import existing Stack projects)
+ Jetbrains IntelliJ IDEA (The free Community Edition is sufficient)
+ IntelliJ Haskell plugin

You can use Stack with any IDE or none. If using IntelliJ, here's a link to the IntelliJ Haskell plugin doc:

[https://github.com/rikvdkleij/intellij-haskell/blob/master/README.md](https://github.com/rikvdkleij/intellij-haskell/blob/master/README.md)

A link to the Stack site: [https://docs.haskellstack.org/en/stable/README/](https://docs.haskellstack.org/en/stable/README/)

## Stack installation

Pick your OS:

### MacOS:

`brew install stack`

### Microsoft Windows:

Use the [64-bit installer](https://get.haskellstack.org/stable/windows-x86_64-installer.exe)

### Linux, BSD, *nix ...

```bash

curl -sSL https://get.haskellstack.org/ | sh

```

or

```bash

wget -qO- https://get.haskellstack.org/ | sh

```


## Compile-and-Run

From the project-root command-line, issue:

`stack build --exec fibonacci-exe`

Result on console:

```bash

fibonacci of 1000
43466557686937456435688527675040625802564660517371780402481729089536555417949051890403879840079255169295922593080322634775209689623239873322471161642996440906533187938298969649928516003704476137795166849228875
M

```
