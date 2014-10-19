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

