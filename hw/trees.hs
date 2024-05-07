data Tree = Nil | Node Tree Int Tree
    deriving (Eq, Ord, Show)

allBalanced :: Int -> [Tree]
allBalanced i = allBalanced2Helper i 0 i


allBalanced2Helper :: Int -> Int -> Int -> [Tree]
-- 0 nodes left to create => return Nil
allBalanced2Helper 0 _ _ = [Nil]
-- i nodes left to create with numbers [start..end] 
allBalanced2Helper totalNodes start end = 
    [ 
        -- Create a possible tree using left and right subtrees
        Node left (start+rootNumber) right 
    |   
        -- For every size of left subtree calculate the number of root
        rootNumber <- map (totalNodes-) leftCounts,
        -- Create a left subtree of size (rootNumber-1) with numbers [start..start+rootNumber-1]
        left <- allBalanced2Helper (rootNumber-1) start (start+rootNumber-1),
        -- Create a right subtree of size (totalNodes-rootNumber) with numbers [start+rootNumber..end]
        right <- allBalanced2Helper (totalNodes-rootNumber) (start+rootNumber) end
    ]
    where        
        -- Find all possible sizes of left subtree to satisfy the constraint
        leftCounts = sizesOfLeftSubtree (totalNodes-1)
        -- From that calculate the size of right subtree
        rightCounts = map ((totalNodes-1)-) leftCounts



-- Utility function to calculate all possible sizes of left subtree
sizesOfLeftSubtree :: Int -> [Int]
sizesOfLeftSubtree i = 
    let half = i `div` 2 in
        if even i then
            [half]
        else
            [half, half+1]
