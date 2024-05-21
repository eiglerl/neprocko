readPuzzle :: IO [[Maybe Int]]
readPuzzle = do
    input <- getContents
    return $ map (map parseCell) (lines input)
    where
        parseCell '-' = Nothing
        parseCell c = Just (read [c])

main :: IO ()
main = do
    lines <- getContents 
    print lines
        

-- solve 
