module Sudoku (Table) where


import Data.List


-- Defining useful types
type Row = [Int]
type Table = [Row]
type Column = [Int]

-- For every cell (x,y) a list of possible moves
type PossibleMoves = [[[Int]]]


-- Utility function to determine if number x is a square,
-- x = n^2 for n in natural numbers
isNumberSquare :: Int -> Bool
isNumberSquare x = sqrtX == fromIntegral (round sqrtX)
    where
        sqrtX = sqrt $ fromIntegral x

-- All possible values in each cell, determined by the lenght of a side of the whole table
possibleValues :: Int -> [Int]
possibleValues tableLen = [1..tableLen]

-- Const value for an empty cell
emptyValue :: Int
emptyValue = 0

-- Utility functions to get row, column, subgrid or a cell
-- could maybe utilize `Data.Array` for O(1) indexing
getRow :: Table -> Int -> Row
getRow table row = table !! row

getCol :: Table -> Int -> Column
getCol table col = map (!! col) table

getSubTable :: Table -> Int -> Int -> Int -> [Int]
getSubTable table row col tableLen = [table !! i !! j | i <- [r..r+subTableSide-1], j <- [c..c+subTableSide-1]]
    where
        subTableSide = round (sqrt (fromIntegral tableLen))
        r = subTableSide * (row `div` subTableSide)
        c = subTableSide * (col `div` subTableSide)

getCell :: Table -> Int -> Int -> Int
getCell table row col = table !! row !! col



-- Function to find all possible numbers for each cell in the whole table
allPossibleMoves :: Table -> Int -> PossibleMoves
allPossibleMoves table tableLen =
    [[
    -- Get value of the cell
    case getCell table row col of
        -- If cell is empty get all possible numbers
        n | n == emptyValue -> getPossibleMovesInCell table row col tableLen
        -- Otherwise it is not empty and there are no possible numbers
        _ -> []
    | col <- [0..tableLen - 1]] | row <- [0..tableLen - 1]]

-- Function to find all possible numbers for a cell
getPossibleMovesInCell :: Table -> Int -> Int -> Int -> [Int]
-- Possible values are `possibleValues` - `takenValues`, where - is a set minus
getPossibleMovesInCell table row col tableLen = filter (not . (`elem` takenValues)) (possibleValues tableLen)
    where
        -- Find all taken numbers in the row, column and subgrid
        rowValues = getRow table row
        colValues = getCol table col
        subTableValues = getSubTable table row col tableLen

        -- Concatenate them
        takenValues = rowValues ++ colValues ++ subTableValues

-- Given a table and possible moves for each cell, assign numbers to empty cell where there is only 1 option
-- and continue solving by calling solve' with the new table
assignCertainMoves :: Table -> PossibleMoves -> Int -> Maybe Table
assignCertainMoves table moves tableLen =
    solve' [[assignCertainValueToCell (getCell table row col) (moves !! row !! col) | col <- [0..tableLen - 1]] | row <- [0..tableLen - 1]] tableLen
    where
        -- Helper function 
        assignCertainValueToCell :: Int -> [Int] -> Int
        assignCertainValueToCell prior moves
            -- that either assigns prior value if there is one
            | prior /= emptyValue = prior
            -- possible value if there is only one option
            | length moves == 1 = head moves
            -- or leaves it empty
            | otherwise = emptyValue

-- Function to check if there are no certain moves
needRandom :: PossibleMoves -> Bool
-- If there is a certain move (any list of possible moves has length one) there is no need to do a random move
needRandom moves = not (any (any (\x -> length x == 1)) moves)

-- A function that finds the best candidate for a random move
-- and calls `tryAllOptionsForCell` function to try all the options using DFS
chooseRandomMove :: Table -> PossibleMoves -> Int -> Maybe Table
chooseRandomMove table moves tableLen =
    case firstShortest moves 0 0 (-1,-1,[0..length table]) of
        Just (row, col, options) -> tryAllOptionsForCell table row col options tableLen
        _ -> Nothing
    where
    -- case first moves 0 0 of
    --     Just (row, col, options) -> tryAll table row col options
    --     _ -> Nothing

    -- Naive version that finds the first empty cell 
    -- first [] _ _ = Nothing
    -- first ([]:rows) r c = first rows (r + 1) 0
    -- first ((col:cols):rows) r c
    --     | length col > 1 = Just (r, c, col)
    --     | otherwise = first (cols:rows) r (c + 1)

    -- Helper function that finds the first cell with least amount of options and returns the position and possible options
    -- arguments are: Table, rowIndex, colIndex, (bestRowIndex, bestColIndex, bestOptions)
    -- There are not any, should not happen
    firstShortest [] _ _ (-1,-1,_) = Nothing
    -- At the end of table, return the best cell
    firstShortest [] _ _ (r, c, shortest) = Just (r, c, shortest)
    -- At the end of row, continue with another one
    firstShortest ([]:rows) r c best = firstShortest rows (r+1) 0 best
    -- Check if current cell is better
    firstShortest ((col:cols):rows) r c (bestR, bestC, shortest)
        | length col > 1 && length col < length shortest = firstShortest (cols:rows) r (c+1) (r,c,col)
        | otherwise = firstShortest (cols:rows) r (c + 1) (bestR, bestC, shortest)

-- Function to try all possible values for a cell using DFS and backtracking
tryAllOptionsForCell :: [[Int]] -> Int -> Int -> [Int] -> Int -> Maybe Table
-- If the list of options is empty there is no solution => return `Nothing`
tryAllOptionsForCell table row col [] _ = Nothing
-- Otherwise try solving a table where cell has value `pos`
tryAllOptionsForCell table row col (pos:possibilities) tableLen =
    case solve' newTable tableLen of
        -- Return the solution if it is valid
        Just solvedTable ->
            Just solvedTable
        -- Otherwise try next option for the cell using recursive call
        Nothing ->
            tryAllOptionsForCell table row col possibilities tableLen
    where
        -- Updated table where cell has value `pos`
        newTable = changeNumberAtIndex table row col pos

-- Function to check if game is over by checking whether all conditions are satisfied
isOver :: Table -> Int -> Bool
isOver table tableLen =
    -- All rows are valid
    all (isValidList tableLen . getRow table) [0..tableLen - 1] &&
    -- All columns are valid
    -- All columns are valid
    -- All columns are valid
    -- All columns are valid
    -- All columns are valid
    -- All columns are valid
    -- All columns are valid
    -- All columns are valid

    -- All columns are valid

    -- All columns are valid

    -- All columns are valid

    -- All columns are valid

    -- All columns are valid

    -- All columns are valid
    -- All columns are valid
    -- All columns are valid

    -- All columns are valid

    -- All columns are valid
    
    -- All columns are valid
    -- All columns are valid
    -- All columns are valid
    -- All columns are valid

    -- All columns are valid

    -- All columns are valid

    -- All columns are valid
    -- All columns are valid

    -- All columns are valid
    all (isValidList tableLen . getCol table) [0..tableLen - 1] &&
    -- All subgrids are valid
    -- All subgrids are valid
    -- All subgrids are valid
    -- All subgrids are valid
    -- All subgrids are valid
    -- All subgrids are valid
    -- All subgrids are valid
    -- All subgrids are valid
    all (isValidList tableLen) [getSubTable table r c tableLen | r <- [0..tableLen - 1], c <- [0..tableLen - 1]]

    where
        -- list is valid iff it contains all possible values 
        isValidList :: Int -> [Int] -> Bool
        isValidList tableLen list = sort list == possibleValues tableLen

-- Check if possible moves are valid
-- by checking if for every empty cell there is at least one possible value
arePossibleMovesValid :: Table -> PossibleMoves -> Bool
arePossibleMovesValid [] [] = True
arePossibleMovesValid ([]:tRows) ([]:mRows) = arePossibleMovesValid tRows mRows
arePossibleMovesValid ((tCol:tCols):tRows) ((mCol:mCols):mRows)
    -- If cell is filled and options are empty
    | tCol /= emptyValue && null mCol = arePossibleMovesValid (tCols:tRows) (mCols:mRows)
    -- Else if cell is empty and options are not empty
    | tCol == emptyValue && not (null mCol) = arePossibleMovesValid (tCols:tRows) (mCols:mRows)
    -- Otherwise it cannot be solved
    | otherwise = False



-- Utility functions that changes one position in a table
changeNumberAtIndex :: Table -> Int -> Int -> Int -> Table
changeNumberAtIndex matrix r c newNum =
    [ [ if (x, y) == (r, c) then newNum else val | (y, val) <- zip [0..] row ] | (x, row) <- zip [0..] matrix ]

-- The main function for the sudoku solver
-- Recieves a table
solve :: Table -> Maybe Table
solve table =
    let
        rowLen = length table
        colLen = length $ head table
        checkLen len xs = length xs == len

    in
        if null table then
            error "Empty table."
        else if rowLen /= colLen then
            error ("The length of the first row (" ++ show colLen ++ ") does not equal the length of the first column (" ++ show rowLen ++ ").")
        else if not $ isNumberSquare rowLen then
            error ("Incorrect sudoku size, has to be n^2 x n^2 for some n. The current size is " ++ show rowLen ++ "x" ++ show rowLen ++ ".")
        else if not $ all (checkLen rowLen) table then
            error "Not all rows have the same length."
        else if not $ all (checkLen colLen) [getCol table c | c <- [0..colLen]] then
            error "Not all columns have the same length."
        else
            solve' table rowLen
        -- -- Starts by checking whether the table has correct size
        -- if 
        --     -- Number of rows has to equal number of columns
        --     rowLen == colLen &&
        --     -- Both of the numbers have to be squares
        --     isNumberSquare rowLen &&
        --     -- Make sure all rows have the same length
        --     all (checkLen rowLen) table &&
        --     -- Make sure all columns have the same length
        --     all (checkLen colLen) [getCol table c | c <- [0..colLen]] 

        --     then
        --         -- Call helper function to actually solve the sudoku
        --         solve' table rowLen
        -- else
        --     -- Otherwise the size is incorrect
        --     error ("Incorrect sudoku size, has to be n^2 x n^2 for some n. Number of rows is " ++ show rowLen ++ ", number of columns is " ++ show colLen)

-- Main solving function
solve' :: Table -> Int -> Maybe Table
solve' table tableLen
    -- Return the table if it is solved
    | isOver table tableLen = Just table
    -- Return `Nothing` if it cannot be solved
    | not (arePossibleMovesValid table moves) = Nothing
    -- Continue with a random move if needed
    | needRandom moves = chooseRandomMove table moves tableLen
    -- Otherwise fill in certain moves
    | otherwise = assignCertainMoves table moves tableLen

    where
        moves = allPossibleMoves table tableLen

