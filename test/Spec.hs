{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Debug.Trace (trace)
import Servant.ConcTest.Utils
import Test.QuickCheck

main :: IO ()
main = do
  -- If you take a list of lists, add the list index to each element, interleave it,
  -- and then gather any of the interleavings, you get back the original list
  let listOfLists :: Gen [[Char]] =
        resize 4 $ listOf $ listOf1 $ choose ('a', 'z')
  let listAndInterleavings :: Gen ([[Char]], [[(Int, Char)]]) = do
        l <- listOfLists
        return (l, interleavings $ addListIndex l)
  -- quickCheck $ verbose $ forAll listAndInterleavings $ \(xs, yss) ->
  quickCheck $ forAll listAndInterleavings $ \(xs, yss) ->
    conjoin [gather ys === xs | ys <- yss]
  where
    -- Convert each list into pairs by zipping the list index to each element:
    -- [[a, b], [c], [d, e]] ~> [[(0, a), (0, b)], [(1, c)], [(2, d), (2, e)]]
    addListIndex xs' | trace ("xs': " ++ show xs') False = undefined
    addListIndex xs' = map (\(t, es) -> zip (repeat t) es) . (zip [0 ..]) $ xs'
