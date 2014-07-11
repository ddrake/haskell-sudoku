Haskell Sudoku
==============

A Sudoku solver in Haskell

Sample Session
--------------

~~~
dow@dow-laptop ~/haskell/sudoku $ ghci
GHCi, version 7.4.1: http://www.haskell.org/ghc/  :? for help
Loading package ghc-prim ... linking ... done.
Loading package integer-gmp ... linking ... done.
Loading package base ... linking ... done.
Prelude> :l Sudoku
[1 of 1] Compiling Sudoku           ( Sudoku.hs, interpreted )
Ok, modules loaded: Sudoku.
*Sudoku> let g = initg ".94...13..............76..2.8..1.....32.........2...6.....5.4.......8..7..63.4..8"
*Sudoku> let g' = cycl g
*Sudoku> gct g'
56
~~~

*56 < 81 (all cells solved), so we're not there yet... stuck! better take a look at the game...*

~~~
*Sudoku> pretty g'
    7         9         4         5         8         2         1         3         6     
    2         6         58        49        3         1         7         48        59    
   1358       15       1358       49        7         6         59        48        2     
    6         8         59        7         1         59        3         2         4     
    4         3         2         8         6         59        59        7         1     
   159        15        7         2         4         3         8         6         59    
   189        2        189        6         5         7         4         19        3     
   359        4        359        1         2         8         6         59        7     
    15        7         6         3         9         4         2         15        8     
*Sudoku> let g1 = guess 2 4 4 g'
~~~

*naughty! - shouldn't be guessing!  (better to use logic)*

~~~
*Sudoku> let g1' = cycl g1
*Sudoku> gct g1'
80
~~~

*80 -- wtf???*

~~~
*Sudoku> pretty g1'
    7         9         4         5         8         2         1         3         6     
    2         6         5         4         3         1         7         8         9     
    3         1         8         9         7         6         5         4         2     
    6         8                   7         1         9         3         2         4     
    4         3         2         8         6         5         9         7         1     
    9         1         7         2         4         3         8         6         5     
    8         2         1         6         5         7         4         9         3     
    3         4         9         1         2         8         6         5         7     
    5         7         6         3         9         4         2         1         8 
~~~

*oh, I see, an empty set of possible values indicates that the guess was wrong!*

~~~    
*Sudoku> let g1 = guess 2 4 9 g'
*Sudoku> let g1' = cycl g1
*Sudoku> gct g1'
81
*Sudoku> pretty g1'
    7         9         4         5         8         2         1         3         6     
    2         6         8         9         3         1         7         4         5     
    3         1         5         4         7         6         9         8         2     
    6         8         9         7         1         5         3         2         4     
    4         3         2         8         6         9         5         7         1     
    1         5         7         2         4         3         8         6         9     
    8         2         1         6         5         7         4         9         3     
    9         4         3         1         2         8         6         5         7     
    5         7         6         3         9         4         2         1         8     
*Sudoku> 
~~~

*yay - guessed right the second time! -- genius!!*