import Data.Maybe

-- Fun of programming
-- Chapter 3 Origami programming

foldL :: (a -> b -> b) -> b -> [a] -> b
foldL _ e [] = e
foldL f e (x:xs) = f x $ foldL f e xs

isort xs = foldL insert [] xs
      where
        insert y [] = [y]
        insert y (x:xs)
               | y < x     = y:x:xs
               | otherwise = x:(insert y xs)

-- Exercise 3.4

insert1 :: Ord a => a -> [a] -> ([a], [a])
insert1 y xs = foldL (\x (l, d) -> 
        if y<x then (x:l, y:x:l) else (x:l, x:d)
        ) ([], [y]) xs

paraL :: (a -> ([a], b) -> b) -> b -> [a] -> b
paraL f e [] = e
paraL f e (x:xs) = f x (xs, paraL f e xs)

-- Exercise 3.5

insert1' y xs = paraL (\x (l, d) ->
         if y<x then y:x:l else x:d
         ) [y] xs

-- Exercise 3.6

unfoldL' :: (b -> Maybe (a,b)) -> b -> [a]
--unfoldL' f u = case f u of
--  Nothing -> []
--  Just (x,v) -> x:(unfoldL' f v)
unfoldL' f u = unfoldL (isNothing.f) (\b -> case f b of 
         Just (x,_) -> x) (\b -> case f b of 
         Just (_,v) -> v) u

unfoldL :: (b->Bool) -> (b->a) -> (b->b) -> b -> [a]
unfoldL p f g b = if p b then [] else (f b):(unfoldL p f g (g b))
--unfoldL p f g = unfoldL' (\x -> if p x then Nothing else Just(f x,g x))

-- Exercise 3.8

foldL' :: (Maybe (a, b) -> b) -> [a] -> b 
--foldL' f [] = f Nothing 
--foldL' f (x:xs) = f (Just (x, foldL' f xs))
foldL' f = foldL (\x acc -> f $ Just (x, acc)) (f Nothing)

--foldL f e = foldL' (\x -> case x of
--      Nothing       -> e
--      Just (a, acc) -> f a acc)

sum' = foldL' (\x -> case x of
    Nothing       -> 0
    Just (a, acc) -> a + acc)

-- Exercise 3.9

foldLargs :: (a -> b -> b) -> b -> (Maybe (a, b) -> b)
foldLargs f e = (\x -> case x of
          Nothing       -> e
          Just (x, acc) -> f x acc)

unfoldLargs :: (b -> Bool) -> (b -> a) -> (b -> b) -> (b -> Maybe (a, b))
unfoldLargs p f g = (\x -> if p x then Nothing else Just (f x, g x))

-- Exercise 3.10

deleteL :: Eq a => a -> [a] -> [a]
deleteL y = paraL (\x (xs, ys) ->
        if x==y then xs else x:ys) []

-- Exercise 3.11

minimumL :: Ord a => [a] -> a
minimumL (x:xs) = foldL min x xs

delmin :: Ord a => [a] -> Maybe (a, [a])
delmin xs = paraL (\x (ac, m) -> case m of
       Nothing        -> if x==y then Just (y, ac) else Nothing
       Just (y', ac') -> Just (y', x:ac')
       ) Nothing xs
       where y = minimumL xs

-- Exercise 3.13

insert :: Ord a => a -> [a] -> [a]
--    insert y Nil = wrap y
--    insert y (Cons x xs) | y < x = Cons y (Cons x xs)
--                         | otherwise = Cons x (insert y xs)

--insert x xs = unfoldL' insert' (Just x, xs)
--       where
--        insert' :: Ord a => (Maybe a, [a]) -> Maybe (a, (Maybe a, [a]))
--        insert' (_, [])             = Nothing
--        insert' (Nothing, y:ys)     = Just (y, (Nothing, ys))
--        insert' (Just n, xs@(y:ys)) = if y > n then Just (n, (Nothing, xs)) else Just (y, (Just n, ys))

-- Exercise 3.14

apoL' :: (b -> Maybe (a, Either b [a])) -> b -> [a]
apoL' f u = case f u of
      Nothing            -> []
      Just (x, Left v)   -> x:(apoL' f v)
      Just (x, Right xs) -> x:xs

insert x xs = apoL' insert' (Just x, xs)
       where insert' :: Ord a => (Maybe a, [a]) -> Maybe (a, Either (Maybe a, [a]) [a])
             insert' (_, []) = Nothing
             insert' (Nothing, y:ys) = Just (y, Right ys)
             insert' (Just n, xs@(y:ys)) = if y > n then Just (n, Left (Nothing, xs)) else Just (y, Left (Just n, ys))

-- Exercise 3.16

data Nat = Zero | Succ Nat deriving (Show)

foldN :: a -> (a -> a) -> Nat -> a
foldN z s Zero = z
foldN z s (Succ n) = s (foldN z s n)

iter :: Nat -> (a -> a) -> (a -> a)
iter n f x = foldN x f n

-- helper

genN :: Int -> Nat
genN 0 = Zero
genN n = Succ $ genN (n-1)

addN :: Nat -> Nat -> Nat
addN n = foldN n Succ

mulN :: Nat -> Nat -> Nat
mulN = (foldN Zero).addN

powN :: Nat -> Nat -> Nat
powN = (foldN $ Succ Zero).mulN

-- Exercise 3.19

predN :: Nat -> Maybe Nat
predN Zero = Nothing
predN (Succ n) = foldN (Just n) id (Succ Zero)
