module Servant.ConcTest.Utils where

-- | Generate all possible interleavings of the input lists.
-- E.g. @interleavings [[1,2],[3,4]]
-- == [[1,2,3,4],[1,3,4,2],[1,3,2,4],[3,4,1,2],[3,1,2,4],[3,1,4,2]]
--
-- From https://stackoverflow.com/a/41929156
-- NOTE: is there a more efficient way to do this?
interleavings :: [[a]] -> [[a]]
interleavings = go . filter (not . null)
  where
    go [] = [[]]
    go xss = do
      (xssl, x : xs, xssr) <- zippers xss
      (x :) <$> interleavings ([xs | not (null xs)] ++ xssl ++ xssr)
    zippers :: [a] -> [([a], a, [a])]
    zippers = go' []
      where
        go' l (h : r) = (l, h, r) : go' (h : l) r
        go' _ [] = []

-- | Convert a list of tuples to a list of lists, using the integers as indexes
-- in the returned list.
-- Example: gather [(2, a), (0, b), (2, c), (1, d), (0, e), (2, f))]
-- == [[b, e], [d], [a, c, f]]
gather :: [(Int, a)] -> [[a]]
gather [] = []
gather ((i, x) : xs) = insert i x (gather xs)

-- | Insert an element into the list at a given index
insert :: Int -> a -> [[a]] -> [[a]]
-- If the index is zero, prepend the element to the first sublist or create a new sublist if the list is empty
insert 0 x [] = [[x]]
insert 0 x (l : ls) = (x : l) : ls
-- If the index is negative, return the original list
insert i _ ls | i < 0 = ls
-- If the index is positive, recurse on the tail of the list and decrement the index
insert i x (l : ls) = l : insert (i - 1) x ls
-- If the index is larger than the length of the list, append the element to the last sublist or create a new sublist if the list is empty
insert i x [] = replicate i [] ++ [[x]]
