# fibonacci

This short Haskell module contains an example of using a 
State monad to compute a Fibonacci sequence.

The State monad does recurse here, but it doesn't grow the stack. 
The implemention uses uses Integer in lieu of int. A huge sequence 
is workable. For exeample, we've tested with `fibonnaci 500000`, 
resulting in a pages-long numeric result. 
