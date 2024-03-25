{-|
Module:        W04lab
Description:   Week 4 Lab: Functional Data Structures
Copyright: (c) University of Toronto Mississsauga
               CSC324 Principles of Programming Languages, Fall 2023

-}

-- This lists what this module exports. Don't change this!
module W04lab
  (
    alookup, aset, adel
  )
where

-- Remember that you may not add any additional imports
import Test.QuickCheck (Property, quickCheck, (==>))

-- This is a **type alias** to make the type signatures of our
-- functions more easily readable.
-- Any time we write "AssocList" in a type signature, it is
-- identical to writing "[(String, Int)]"
type AssocList = [(String, Int)]


-- | Look up a key in the Association List
alookup :: AssocList -> String -> Int
alookup [] s = -1

alookup assoc s = if fst (head assoc) == s then snd (head assoc) else alookup (tail assoc) s



-- | Set a key in the Association List
aset :: AssocList -> String -> Int -> AssocList
aset assoc k v = if alookup assoc k == -1 then assoc ++ [(k, v)] else map (\(s,x) -> if s == k then (k, v) else (s,x)) assoc


-- | Delete a key in the Association List
adel :: AssocList -> String -> AssocList
adel assoc k = filter(\(s,x) -> s /= k) assoc



-------------------------------------------------------------------------------
-- * Main function (for testing purposes only)
-------------------------------------------------------------------------------

prop_alookup :: Bool
prop_alookup = 1 == alookup [("A", 1)] "A"

prop_aset :: Bool
prop_aset = [("A", 1), ("B", 2), ("C", 3)] == aset[("A", 1), ("B", 2)] "C" 3

prop_aset1 :: Bool
prop_aset1 =  [("A", 1), ("B", 2), ("C", 3)] == aset[("A", 1), ("B", 2), ("C", 33)] "C" 3

prop_adel :: Bool
prop_adel = [("A", 1), ("C", 3)] == adel[("A", 1), ("B", 2), ("C", 3)] "B"

prop_adel1 :: Bool
prop_adel1 = [("A", 1), ("B", 2), ("C", 3)] == adel[("A", 1), ("B", 2), ("C", 3)] "D"

prop_adel2 :: Bool
prop_adel2 = [("A", 1), ("B", 2), ("C", 3)] == adel[("A", 1), ("B", 2), ("C", 3), ("D", 4)] "D"

prop_aset2 :: Bool
prop_aset2 = [("A", 1), ("B", 6), ("C", 3), ("B", 6), ("E", 6)] == aset[("A", 1), ("B", 2), ("C", 3), ("B", 4), ("E", 6)] "B" 6

prop_aset3 :: Bool
prop_aset3 = [("A", 100), ("B", 200), ("C", 300), ("B", 400), ("E", 500)] == aset[("A", 1), ("B", 200), ("C", 300), ("B", 400), ("E", 500)] "A" 100

prop_alookup3 :: Bool
prop_alookup3 = 4 == alookup[("A", 1), ("B", 2), ("C", 3), ("D", 4)] "D"

prop_alookup4 :: Bool
prop_alookup4 = 3 == alookup[("A", 1), ("B", 2), ("C", 3), ("D", 4)] "C"

prop_alookup5 :: Bool
prop_alookup5 = -1 == alookup[("A", 1), ("B", 2), ("C", 3), ("D", 4)] "E"

-- This main function is executed when you compile and run this Haskell file.
-- It runs the QuickCheck tests; we'll talk about "do" notation much later in
-- the course, but for now if you want to add your own tests, just define them
-- above, and add a new `quickCheck` line below.
main :: IO ()
main = do
  quickCheck prop_alookup
  -- TODO: add other tests
  quickCheck prop_aset
  quickCheck prop_aset1
  quickCheck prop_adel
  quickCheck prop_adel1
  quickCheck prop_adel2
  quickCheck prop_aset2
  quickCheck prop_aset3
  quickCheck prop_alookup3
  quickCheck prop_alookup4
  quickCheck prop_alookup5

