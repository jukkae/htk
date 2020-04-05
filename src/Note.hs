module Note where

import Data.List


type Note = Int
data Tonality = Tonality
  { notes :: [Note]
  , tonic :: Note
  , allNotes :: [Note]
  }

data Trichord = Trichord Note Note Note

data Interval = Interval
  { a :: Note
  , b :: Note
  }

type Chord = [Note]

printTrichord :: Trichord -> String
printTrichord (Trichord a b c) =
  concat ["[", printNote a, ":", printNote b, ":", printNote c, "]"]

printInterval :: Interval -> String
printInterval (Interval a b) =
  concat ["[", printNote a, ":", printNote b, "]"]

-- Unique combinations of k items from ns
combinations k ns = filter ((k==).length) $ subsequences ns

isHarmonious :: Trichord -> Bool
isHarmonious (Trichord a b c) =
  ((x `mod` 12 == 3 || x `mod` 12 == 4) && (y `mod` 12 == 3 || y `mod` 12 == 4)) ||
  ((x `mod` 12 == 3 || x `mod` 12 == 4) && (z `mod` 12 == 3 || z `mod` 12 == 4)) ||
  ((y `mod` 12 == 3 || y `mod` 12 == 4) && (z `mod` 12 == 3 || z `mod` 12 == 4))
  where
    x = b - a
    y = c - b
    z = a - c

-- TODO: This will blow up with any list shorter than 3
trichordFromNotes :: [Note] -> Trichord
trichordFromNotes ns = Trichord a b c where
  a = head ns
  b = head (tail ns)
  c = head (tail (tail ns))

intervalFromNotes :: [Note] -> Interval
intervalFromNotes ns = Interval a b where
  a = head ns
  b = head (tail ns)

rotations :: [a] -> [[a]]
rotations xs = take (length xs) (iterate (\(y:ys) -> ys ++ [y]) xs)

-- Permutations of a list
perms :: [a] -> [[a]]
perms = foldr (\ x -> concatMap (rotations . (x :))) [[]]

triSpan :: Trichord -> Trichord -> Ordering
triSpan (Trichord a1 _ c1) (Trichord a2 _ c2) = compare dist1 dist2 where
  dist1 = (c1 - a1) `nMod` 12
  dist2 = (c2 - a2) `nMod` 12

-- Ehh, rewrite this at some point
trichordRotations :: Trichord -> [Trichord]
trichordRotations (Trichord a b c) =
  [
    Trichord a b c,
    Trichord b c a,
    Trichord c a b
  ]

packTri :: Trichord -> Trichord
packTri tri = minimumBy triSpan ascendingForms where
  ascendingForms = trichordRotations tri -- tri must be packed within single octave, in order
-- packTri tri = head (sortBy triSpan (filter isAscending (perms tri)))

packTris :: [Trichord] -> [Trichord]
packTris = map packTri

pseudotonic :: Trichord -> Trichord -> Ordering
pseudotonic (Trichord a1 _ c1) (Trichord a2 _ c2) = compare a1 a2

packAndOrderTris :: [Trichord] -> [Trichord]
packAndOrderTris tris = sortBy pseudotonic (packTris tris)

printNote :: Note -> String
printNote 10 = "T"
printNote 11 = "E"
printNote n = show n

printTonalityNotes :: Tonality -> String
printTonalityNotes w =
  concat [maybePrintNote n | n <- allNotes w] where
    maybePrintNote m =
      if m `elem` notes w then printNote m
      else "X"

-- 12-EDO assumed!
transposeTonality :: Tonality -> Int -> Tonality
transposeTonality w amount = Tonality newNotes newTonic allPossibleNotes where
  newNotes = map (addMod12 amount) (notes w)
  newTonic = addMod12 (tonic w) amount
  addMod12 a b = (a + b) `mod` 12

flipSelection :: Tonality -> Tonality
flipSelection w = Tonality newNotes newTonic allPossibleNotes where
  newNotes = [x | x <- [0..11], x `notElem` notes w]
  newTonic = tonic w

resetTonality :: Tonality -> Tonality
resetTonality w = initialTonality

toggleSelected :: Tonality -> Int -> Tonality
toggleSelected w index = Tonality newNotes newTonic allPossibleNotes where
  newNotes =
    if index `elem` ns then filter (/= index) ns
    else sort ns ++ [index]
  ns = notes w
  newTonic = tonic w

initialNotes :: [Note]
initialNotes = [0, 2, 3, 5, 7, 8, 10]

allPossibleNotes :: [Note]
allPossibleNotes = [0..11]

initialTonic :: Note
initialTonic = 0

initialTonality :: Tonality
initialTonality = Tonality initialNotes initialTonic allPossibleNotes

getTrichords :: Tonality -> [Trichord]
getTrichords t = map trichordFromNotes (combinations 3 (notes t))

getIntervals :: Tonality -> [Interval]
getIntervals t = map intervalFromNotes (combinations 2 (notes t))

-- count :: Eq a => a -> [a] -> Int
-- count x = length . filter (x==)
countBy p xs = length (filter p xs)

getIntervalVector :: Tonality -> [Int]
getIntervalVector t =
  [countBy (\x -> intervalLength x ==  y) intervals | y <- [1..6]]
  where intervals = getIntervals t

-- let x = a `mod` b
-- => (x >= 0 && x < 12) || x > -b
-- => this function is always positive
nMod a b = if a `mod` b < 0 then (a `mod` b) + b else a `mod` b

intervalLength :: Interval -> Int
intervalLength (Interval a b) =
  min ((b - a) `nMod` 12) ((a - b) `nMod` 12)
