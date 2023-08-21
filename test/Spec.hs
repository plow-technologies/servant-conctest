{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Debug.Trace (trace)
import Servant.ConcTest.Utils
import Test.QuickCheck

-- | If you take a list of lists, add the list index to each element, interleave it,
-- and then gather any of the interleavings, you get back the original list
prop_interleave_then_gather_id :: (Show a, Eq a) => [[a]] -> Property
prop_interleave_then_gather_id xs =
  forAll (elements $ interleavings $ addListIndex xs) $ \ys ->
    gather ys === xs
  where
    -- Convert each list into pairs by zipping the list index to each element:
    -- [[a, b], [c], [d, e]] ~> [[(0, a), (0, b)], [(1, c)], [(2, d), (2, e)]]
    addListIndex xs' | trace ("xs': " ++ show xs') False = undefined
    addListIndex xs' = map (\(t, es) -> zip (repeat t) es) . (zip [0 ..]) $ xs'

main :: IO ()
main = do
  let listOfLists :: Gen [[Char]] = resize 3 $ listOf $ listOf1 $ choose ('a', 'z')
  quickCheck $ verbose $ forAll listOfLists prop_interleave_then_gather_id
