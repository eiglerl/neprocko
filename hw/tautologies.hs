data Prop =
    Const Bool
  | Var Char
  | Not Prop
  | And Prop Prop
  | Or Prop Prop
  deriving Show

type Model = [(Char, Bool)]

-- Prop is taut iff for every possible model prop is True
isTaut :: Prop -> Bool
isTaut p = and [eval p model | model <- models]
    where
        vars = getVariables p
        models = allModels vars

getVariables :: Prop -> [Char]
getVariables (Var a) = [a]
getVariables (Not p) = getVariables p
getVariables (And p q) = getVariables p ++ getVariables q
getVariables (Or p q) = getVariables p ++ getVariables q

allModels :: [Char] -> [Model]
allModels [] = []
allModels [a] = [[(a, True)], [(a, False)]]
allModels (v:vars) =
    map ((v, True):) models ++ map ((v, False):) models
    where
        models = allModels vars

eval :: Prop -> Model -> Bool
eval (Var a) model = (a,True) `elem` model
eval (Not p) model = not $ eval p model
eval (And p q) model = eval p model && eval q model
eval (Or p q) model = eval p model || eval q model

