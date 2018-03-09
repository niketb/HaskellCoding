> data Tree v =
>     Empty
>   | Node v (Tree v) (Tree v)
>   deriving (Show)

> findT :: Ord v => v -> Tree v -> Maybe v
> findT _ Empty = Nothing
> findT v (Node val left right) =
>   if val == v then
>     Just val
>   else if v < val then
>     findT v left
>   else
>     findT v right

> instance Functor Tree where
> fmap f (Empty) = Empty
> fmap f (Node val left right) = Node (f val) (Main.fmap f left) (Main.fmap f right)

> main :: IO ()
> main = do
>   putStrLn $ show $ Main.fmap (+1) (Node 3 (Node 1 Empty Empty) (Node 7 (Node 4 Empty Empty) Empty)) 

Your job is to add support for fmap to this tree, so that the following call works:

fmap (+1) (Node 3 (Node 1 Empty Empty) (Node 7 (Node 4 Empty Empty) Empty))