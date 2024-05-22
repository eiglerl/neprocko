## Sudoku Solver

The `Sudoku` module, contained within the `sudoku.hs` file, encapsulates a solver for Sudoku puzzles. It introduces a `Table` type, represented by `[[Int]]`, to depict Sudoku boards. At the core of the module lie two functions: `solve` and `solveAll`. Notably, the solver is proficient in tackling Sudoku puzzles of dimensions $n^2$ x $n^2$.

### solve :: Table -> Maybe Table

The `solve` function takes a Sudoku table as input (`Table`) and returns a result of type `Maybe Table`. This result either presents the solved board or indicates impossibility (`Nothing`) if a solution cannot be derived. Should the input table be incorrectly defined, the function raises errors accompanied by explanatory messages. 

### solveAll :: Table -> [Table]

The `solveAll` function takes a Sudoku table as input (`Table`) and returns a list of all possible solved boards. If no solutions exist, it returns an empty list. This function is useful for finding multiple solutions to a given Sudoku puzzle, if they exist.

### Example Boards

The file `sudoku_examples.hs` features meticulously crafted Sudoku boards, accurately defined and readily applicable for solving via the `solve` or `solveAll` functions. Users can effortlessly invoke these boards for resolution by calling `solve SudokuExamples.NAMEOFBOARD` or `solveAll SudokuExamples.NAMEOFBOARD`. The provided examples encompass puzzles of dimensions $4$ x $4$, $9$ x $9$, and $16$ x $16$, spanning varying levels of difficulty. However, users should exercise caution regarding the computational demands, especially when dealing with exceedingly challenging Sudoku puzzles larger than $9$ x $9$.
