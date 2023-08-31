module MergeSort where


merge :: (Ord a) => [a] -> [a] -> [a]
merge l1 l2 = case (l1, l2) of
  ([], l) -> l
  (l, []) -> l
  (x:xs, y:ys)
    | x < y     -> x : (merge xs (y:ys))
    | otherwise -> y : (merge (x:xs) ys)

mergeInPairs :: (Ord a) => [[a]] -> [[a]]
mergeInPairs ll = case ll of
  []       -> []
  l1:[]    -> l1:[]
  l1:l2:ls -> merge l1 l2 : mergeInPairs ls

splitIntoSingletons :: (Ord a) => [a] -> [[a]]
splitIntoSingletons l = case l of
  [] -> []
  x:xs -> [x]:splitIntoSingletons xs

mergeSteps :: (Ord a) => [[a]] -> [a]
mergeSteps ll = case nextStep of
  _:_:_ -> mergeSteps nextStep
  [l]    -> l
  []     -> []
  where nextStep = mergeInPairs ll

mergeSort :: (Ord a) => [a] -> [a]
mergeSort l = mergeSteps (splitIntoSingletons l)