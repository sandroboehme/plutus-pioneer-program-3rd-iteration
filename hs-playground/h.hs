import Text.Read

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert newNodeValue EmptyTree = singleton newNodeValue -- deconstructing "a -> Tree a" and return a singleton with newNodeValue
treeInsert newNodeValue (Node a left right) -- deconstructing "a -> Tree a"
    | newNodeValue == a = Node newNodeValue left right -- call the Node constructor with the value and two trees
    | newNodeValue < a  = Node a (treeInsert newNodeValue left) right
    | newNodeValue > a  = Node a left (treeInsert newNodeValue right)

main :: IO ()
main = putStrLn "Hello World!"

-- :t putStrLn
-- putStrLn :: String -> IO ()
-- ">>" takes the first recipe, throws away the result (which is "()") and executes the second recipe.
continueWithStrOutput = putStrLn "Hello" >> putStrLn "World"


--readEitherSandro Read a :: String -> Either String a
--readEitherSandro x = case readMaybe x of
--  Nothing -> Left $ x ++ " cannot be read."
--  Just x -> Right x

mkValidator :: () -> (Bool, Bool) -> () -> Bool
mkValidator _ (a, b) _ = a == b

data MyRedeemer = MyRedeemer
    { flag1 :: Bool
    , flag2 :: Bool
    } deriving (Show)
