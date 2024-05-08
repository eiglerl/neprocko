module SudokuExamples where

import Sudoku (Table)

example :: Table
example =
    [ [5, 3, 0, 0, 0, 0, 0, 0, 0]
    , [6, 2, 4, 1, 9, 5, 0, 0, 0]
    , [1, 9, 8, 0, 0, 0, 0, 6, 0]
    , [8, 0, 0, 0, 6, 0, 0, 0, 3]
    , [4, 0, 0, 8, 0, 3, 0, 0, 1]
    , [7, 0, 0, 0, 2, 0, 0, 0, 6]
    , [0, 6, 0, 0, 0, 0, 2, 8, 0]
    , [0, 0, 0, 4, 1, 9, 0, 0, 5]
    , [0, 0, 0, 0, 8, 0, 0, 7, 9]
    ]

easy :: Table
easy =
    [ [0, 6, 2, 4, 0, 5, 7, 3, 0]
    , [0, 8, 0, 7, 0, 3, 2, 0, 9]
    , [0, 7, 0, 9, 2, 0, 6, 5, 0]
    , [0, 0, 1, 6, 0, 0, 8, 0, 4]
    , [0, 0, 9, 0, 3, 1, 5, 0, 7]
    , [0, 0, 0, 0, 0, 0, 0, 0, 3]
    , [4, 0, 0, 0, 0, 7, 0, 0, 2]
    , [2, 1, 0, 0, 0, 0, 4, 0, 0]
    , [7, 0, 3, 2, 0, 4, 9, 0, 6]
    ]

veryEasy :: Table
veryEasy =
    [ [1, 2, 3, 6, 7, 8, 9, 4, 5]
    , [5, 8, 4, 2, 3, 9, 7, 6, 1]
    , [9, 6, 7, 1, 4, 5, 3, 2, 8]
    , [3, 7, 2, 4, 6, 1, 5, 8, 9]
    , [6, 9, 1, 5, 8, 3, 2, 7, 4]
    , [4, 5, 8, 7, 9, 2, 6, 1, 3]
    , [8, 3, 6, 9, 2, 4, 1, 5, 7]
    , [2, 1, 9, 8, 5, 7, 4, 3, 6]
    , [7, 4, 5, 3, 1, 6, 8, 9, 2]
    ]
veryEasy2 :: Table
veryEasy2 =
    [ [1, 2, 3, 6, 7, 8, 9, 4, 5]
    , [5, 8, 4, 2, 3, 9, 7, 6, 1]
    , [9, 6, 7, 1, 4, 5, 3, 2, 8]
    , [3, 7, 2, 4, 6, 1, 5, 8, 9]
    , [6, 9, 1, 0, 0, 3, 2, 7, 4]
    , [4, 5, 8, 7, 9, 2, 6, 1, 3]
    , [8, 3, 6, 9, 2, 4, 1, 5, 7]
    , [2, 1, 9, 0, 0, 7, 4, 3, 6]
    , [7, 4, 5, 3, 1, 6, 8, 9, 2]
    ]

test2 :: Table
test2 =
    [ [5, 0, 0, 1, 0, 6, 0, 0, 4]
    , [0, 8, 0, 0, 4, 5, 3, 9, 0]
    , [0, 0, 0, 8, 3, 2, 0, 5, 0]
    , [3, 1, 0, 0, 8, 4, 6, 0, 2]
    , [4, 0, 0, 0, 0, 0, 0, 3, 1]
    , [2, 7, 0, 0, 0, 0, 0, 0, 9]
    , [6, 2, 0, 0, 0, 8, 4, 0, 5]
    , [8, 5, 1, 0, 0, 0, 7, 0, 3]
    , [0, 4, 0, 5, 0, 0, 0, 2, 0]
    ]

medium :: Table
medium =
    [ [9, 5, 0, 0, 4, 2, 0, 7, 8]
    , [0, 0, 2, 0, 0, 8, 0, 0, 1]
    , [0, 3, 0, 0, 0, 0, 0, 0, 0]
    , [0, 8, 0, 0, 0, 0, 0, 0, 0]
    , [1, 2, 0, 0, 0, 0, 4, 0, 7]
    , [0, 4, 6, 2, 0, 0, 5, 0, 0]
    , [0, 0, 0, 0, 5, 1, 0, 6, 0]
    , [0, 0, 7, 0, 2, 6, 3, 5, 0]
    , [0, 0, 0, 0, 7, 3, 0, 0, 9]
    ]

hard :: Table
hard =
    [ [8, 0, 6, 0, 0, 0, 3, 0, 0]
    , [0, 4, 0, 1, 0, 2, 0, 5, 0]
    , [0, 9, 2, 5, 0, 0, 0, 0, 0]
    , [0, 0, 0, 0, 1, 3, 0, 4, 0]
    , [0, 0, 0, 0, 5, 0, 0, 6, 3]
    , [4, 2, 0, 0, 8, 7, 0, 0, 0]
    , [0, 0, 9, 0, 0, 0, 7, 8, 1]
    , [0, 0, 0, 0, 0, 0, 0, 3, 9]
    , [0, 0, 4, 8, 9, 0, 5, 0, 0]
    ]

hard2 :: Table
hard2 =
    [ [0, 0, 8, 9, 1, 0, 4, 0, 7]
    , [0, 0, 0, 0, 0, 7, 0, 0, 5]
    , [1, 0, 7, 2, 5, 4, 0, 0, 0]
    , [4, 7, 0, 0, 9, 0, 2, 5, 6]
    , [8, 0, 0, 0, 2, 0, 0, 0, 9]
    , [0, 2, 0, 0, 0, 5, 8, 0, 1]
    , [6, 0, 5, 0, 7, 0, 1, 3, 2]
    , [0, 1, 9, 5, 3, 0, 0, 0, 0]
    , [0, 0, 0, 1, 0, 0, 5, 9, 0]
    ]

extreme :: Table
extreme =
    [ [5, 0, 0, 9, 0, 0, 0, 7, 0]
    , [0, 6, 0, 0, 0, 0, 9, 0, 4]
    , [8, 0, 0, 0, 0, 0, 0, 0, 5]
    , [7, 5, 1, 0, 0, 0, 0, 0, 8]
    , [6, 0, 0, 2, 0, 0, 5, 0, 0]
    , [0, 8, 0, 0, 0, 0, 0, 0, 1]
    , [9, 0, 0, 0, 0, 0, 3, 0, 0]
    , [0, 0, 0, 0, 4, 0, 0, 0, 0]
    , [0, 0, 0, 5, 0, 1, 0, 0, 0]
    ]

onlyZeros :: Table
onlyZeros =
    [ [0, 0, 0, 0, 0, 0, 0, 0, 0]
    , [0, 0, 0, 0, 0, 0, 0, 0, 0]
    , [0, 0, 0, 0, 0, 0, 0, 0, 0]
    , [0, 0, 0, 0, 0, 0, 0, 0, 0]
    , [0, 0, 0, 0, 0, 0, 0, 0, 0]
    , [0, 0, 0, 0, 0, 0, 0, 0, 0]
    , [0, 0, 0, 0, 0, 0, 0, 0, 0]
    , [0, 0, 0, 0, 0, 0, 0, 0, 0]
    , [0, 0, 0, 0, 0, 0, 0, 0, 0]
    ]

unsolvable :: Table
unsolvable =
    [ [2, 0, 0, 9, 0, 0, 0, 0, 0]
    , [0, 0, 0, 0, 0, 0, 0, 2, 0]
    , [0, 0, 0, 0, 0, 1, 0, 0, 0]
    , [5, 0, 2, 6, 0, 0, 4, 0, 7]
    , [0, 0, 0, 0, 0, 4, 1, 0, 0]
    , [0, 0, 0, 0, 9, 8, 0, 2, 3]
    , [0, 0, 0, 0, 0, 3, 0, 8, 0]
    , [0, 0, 5, 0, 1, 0, 0, 0, 0]
    , [0, 0, 7, 0, 0, 0, 0, 0, 0]
    ]

hardest :: Table
hardest =
    [ [8, 0, 0, 0, 0, 0, 0, 0, 0]
    , [0, 0, 3, 6, 0, 0, 0, 0, 0]
    , [0, 7, 0, 0, 9, 0, 2, 0, 0]
    , [0, 5, 0, 0, 0, 7, 0, 0, 0]
    , [0, 0, 0, 0, 4, 5, 7, 0, 0]
    , [0, 0, 0, 1, 0, 0, 0, 3, 0]
    , [0, 0, 1, 0, 0, 0, 0, 6, 8]
    , [0, 0, 8, 5, 0, 0, 0, 1, 0]
    , [0, 9, 0, 0, 0, 0, 4, 0, 0]
    ]

recodex1 :: Table
recodex1 = [[3,0,4,0],[0,1,0,3],[2,3,0,0],[1,0,0,2]]
recodex2 :: Table
recodex2 = [[3,0,4,0],[0,1,0,3],[2,3,0,0],[4,0,0,2]] 
recodex3 :: Table
recodex3 = [[0,0,0,0],[2,0,3,0],[0,1,0,4],[0,0,0,0]] 

tough16x16 :: Table
tough16x16 =
    [ [0, 6, 0, 0, 0, 0, 14, 0, 4, 0, 0, 0, 9, 8, 5, 10]
    , [16, 0, 0, 0, 13, 2, 0, 0, 0, 0, 3, 0, 6, 0, 0, 0]
    , [0, 7, 14, 8, 6, 0, 0, 0, 12, 0, 0, 0, 2, 0, 0, 0]
    , [5, 2, 0, 0, 0, 0, 0, 12, 0, 0, 0, 0, 4, 0, 0, 1]
    , [12, 9, 0, 0, 0, 11, 0, 13, 7, 0, 0, 0, 0, 0, 1, 0]
    , [0, 15, 0, 0, 7, 0, 0, 0, 2, 8, 0, 0, 13, 4, 0, 5]
    , [2, 0, 0, 0, 0, 0, 0, 0, 1, 6, 0, 0, 0, 0, 15, 0]
    , [0, 0, 5, 13, 0, 0, 9, 0, 0, 0, 0, 3, 0, 0, 7, 0]
    , [0, 3, 0, 14, 0, 6, 7, 0, 0, 16, 0, 0, 0, 11, 0, 0]
    , [0, 0, 0, 0, 0, 0, 0, 9, 6, 2, 1, 0, 0, 0, 16, 0]
    , [0, 0, 6, 0, 0, 0, 16, 11, 0, 7, 4, 0, 10, 5, 0, 0]
    , [7, 0, 11, 16, 4, 15, 3, 0, 0, 0, 8, 0, 0, 12, 0, 0]
    , [4, 0, 0, 0, 15, 0, 0, 7, 11, 0, 0, 0, 0, 0, 0, 0]
    , [0, 0, 0, 0, 0, 0, 0, 0, 0, 5, 0, 12, 0, 7, 0, 3]
    , [0, 0, 0, 0, 0, 5, 6, 10, 0, 0, 0, 13, 16, 15, 0, 0]
    , [0, 0, 0, 5, 12, 14, 0, 2, 0, 0, 0, 1, 11, 9, 0, 4]
    ]

medium16x16 :: Table
medium16x16 = 
    [ [3, 0, 0, 0, 0, 0, 5, 0, 16, 13, 0, 12, 2, 8, 11, 14]
    , [12, 7, 0, 0, 15, 4, 0, 0, 0, 14, 11, 1, 0, 0, 5, 0]
    , [0, 5, 0, 0, 0, 0, 0, 0, 0, 4, 0, 0, 16, 9, 0, 0]
    , [0, 0, 0, 14, 7, 0, 0, 13, 0, 0, 0, 8, 0, 1, 0, 4]
    , [11, 0, 0, 16, 0, 9, 4, 15, 6, 0, 0, 10, 0, 0, 14, 0]
    , [9, 15, 4, 1, 5, 0, 0, 0, 0, 0, 0, 11, 0, 6, 0, 0]
    , [0, 14, 10, 0, 0, 8, 13, 6, 1, 0, 12, 16, 3, 0, 0, 0]
    , [0, 0, 13, 2, 1, 14, 0, 0, 4, 0, 5, 7, 9, 16, 12, 0]
    , [0, 13, 0, 0, 4, 1, 0, 5, 15, 0, 0, 9, 8, 0, 0, 0]
    , [7, 0, 0, 10, 3, 16, 0, 0, 0, 0, 8, 4, 0, 0, 0, 5]
    , [2, 1, 0, 0, 0, 6, 0, 12, 0, 0, 10, 0, 0, 0, 7, 3]
    , [0, 0, 0, 8, 0, 0, 0, 2, 0, 16, 0, 0, 0, 10, 9, 13]
    , [0, 0, 14, 5, 10, 13, 0, 0, 0, 0, 16, 15, 11, 0, 0, 0]
    , [0, 16, 11, 0, 8, 5, 0, 3, 0, 1, 0, 0, 0, 12, 0, 0]
    , [0, 2, 0, 0, 16, 0, 0, 0, 8, 3, 0, 0, 0, 0, 1, 0]
    , [13, 9, 8, 0, 12, 0, 0, 4, 0, 10, 0, 6, 5, 7, 0, 0]
    ]