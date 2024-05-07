import System.IO
import GHC.Base (VecElem(Int16ElemRep))
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo(withFullyStaticExe))

lookup2 :: Eq a => a -> [(a,b)] -> [(a,c)] -> Maybe (b,c)
lookup2 a ab ac = case (lookup a ab, lookup a ac) of
    (Just b, Just c) -> Just (b,c)
    _                -> Nothing

lookupChain, lookupChain' :: (Eq a, Eq b) => a -> [(a,b)] -> [(b,c)] -> Maybe (b,c)
lookupChain a ab bc = case lookup a ab of
    Nothing -> Nothing
    Just b -> case lookup b bc of
        Nothing -> Nothing
        Just c -> Just (b,c)

lookupChain' a ab bc = lookup a ab `andThen` \b -> lookup b bc `andThen` \c -> done (b,c)


andThen :: Maybe a -> (a -> Maybe b) -> Maybe b
andThen Nothing _ = Nothing
andThen (Just a) f = f a


done :: a -> Maybe a
done = Just

-- main :: IO ()
-- main = putStrLn "What's your name?" >>= \_ -> getLine >>= \s -> putStrLn ("Hello " ++ s)
-- main = do 
--     _ <- putStrLn "What's your name?"
--     s <- getLine
--     putStrLn ("Hello " ++ s)


main :: IO ()
-- main = do
--     file <- readFile "cviceni/numbers.txt"
--     let
--         str = lines file
--         ints = map read str :: [Int]
--         total = show $ sum ints
    
--     putStrLn total


main = do
    nums <- readFile "cviceni/numbers.txt"
    let sum' :: String -> Int
        sum' = sum . map read . lines
    print $ sum' nums


withFile' :: FilePath -> IOMode -> (Handle -> IO r) -> IO r
withFile' path mode f = do
    h <- openFile path mode
    r <- f h
    hClose h
    pure r