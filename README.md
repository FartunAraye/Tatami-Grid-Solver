# Tatami-Grid-Solver
A Haskell program that solves tatami tile puzzles using backtracking algorithms.
What it does - Tatami puzzles involve filling a grid with rectangular tiles (each covering 2 cells) where no four different tiles can meet at a single corner point. This solver finds valid arrangements automatically.

Features:
Pointer-based navigation - Move through the grid in all four directions (up, down, left, right)
Tile placement functions - Place tatami tiles horizontally or vertically from any position
Backtracking solver - Automatically explores possible tile placements and backtracks when constraints are violated
Visual grid display - Shows the grid with ANSI color highlighting for the current pointer position

How it works
The solver uses a recursive backtracking algorithm that:
Finds the next empty cell in the grid
Tries placing a tile horizontally or vertically
Checks if the placement violates the "no four different corners" rule
Recursively solves the rest of the grid
Backtracks if no valid solution is found
