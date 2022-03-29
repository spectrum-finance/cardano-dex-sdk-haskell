module Common.Data.List.Combinators
  (indexOf
  ) where

indexOf :: (Eq a, Integral s) => a -> [a] -> s
indexOf a as = indexOf' a as 0
  where
    indexOf' :: (Eq a, Integral s) => a -> [a] -> s -> s
    indexOf' _ [] _ = -1
    indexOf' a' (x:xs) s
      | a' == x   = s
      | otherwise = indexOf' a' xs (s + 1)
