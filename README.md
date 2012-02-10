# A* Search Algorithm 8 Tile Puzzle Solver


# How to use

1. Download [DrRacket](http://racket-lang.org/download/)
2. Open `solver.rkt`
3. Click `Run`

In the interactions window you can solve puzzles like this:

```racket
(SOLVE '(1 2 3 4 5 6 7 8 0))
```

The array represents a puzzle configuration like this

```
1 2 3
4 5 6
7 8 0
```

where `0` is the blank tile. The above puzzle also happens to be the goal state. I've included a few sample puzzles
that run automatically, look at those for guidance.