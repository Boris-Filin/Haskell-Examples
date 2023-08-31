module TreeDraw where

-- Binary and Rose tree declarations
-- data BinTree a = Null | BinNode (BinTree a) a (BinTree a)
--   deriving Show

data DrawTree a = Empty | DrawNode a [DrawTree a]


-- Conversion of bin tree to rose tree to avoid separate drawers for each
-- binTreeToRose :: BinTree a -> DrawTree a
-- binTreeToRose b = case b of
--   Null -> Empty
--   BinNode Null node Null -> DrawNode node []
--   BinNode Null node r -> DrawNode node [binTreeToRose r]
--   BinNode l node Null -> DrawNode node [binTreeToRose l]
--   BinNode l node r -> DrawNode node [binTreeToRose l, binTreeToRose r]


-- Some example trees to test the program
testTree :: DrawTree Int
testTree = DrawNode 1 [DrawNode 1 [DrawNode 2 [], DrawNode 3 [DrawNode 4 [], DrawNode 5 []]]]

-- testBinTree :: BinTree Int
-- testBinTree =
--   BinNode (BinNode (BinNode (BinNode Null 8 Null) 4 (BinNode Null 9 Null)) 2 (
--   BinNode (BinNode Null 10 Null) 5 (BinNode Null 11 Null))) 1 (
--   BinNode (BinNode Null 6 (BinNode Null 13 Null)) 3 (BinNode (BinNode Null 14 Null) 7 Null))

testTree2 :: DrawTree Int
testTree2 = DrawNode 1 [DrawNode 2 [], DrawNode 3 [DrawNode 5 [], DrawNode 6 [
  DrawNode 7 []]], DrawNode 4 []]


-- Root method that interacts with IO to actually output the whole thing
showTree :: (Show a) => DrawTree a -> IO ()
showTree t = putStrLn $ "\n" ++ drawTree t

-- An entry point for binary trees to avoid necessity of conversion
-- showBinTree :: (Show a) => BinTree a -> IO()
-- showBinTree t = showTree $ binTreeToRose t

-- A thing that takes a tree and returns the image as a single string with newlines
drawTree :: (Show a) => DrawTree a -> String
drawTree t = foldr (\s r -> ' ' : s ++ "\n" ++ r) "" $ drawNode t

-- A thing that does representations of leaf nodes as well as an empty tree
drawNode :: (Show a) => DrawTree a -> [String]
drawNode t = case t of
  Empty -> ["Empty"]
  DrawNode n [] -> [show n]
  DrawNode n children -> drawParent n children

-- This draws every intermediate node as well as its connections to children
drawParent :: (Show a) => a -> [DrawTree a] -> [String]
drawParent node children = nodeString : connectionsString : mergedChildren
  where
    strChildren = map drawNode children
    mergedChildren = mergeNodes strChildren

    firstLine = mergedChildren !! 0
    lineLength = length (firstLine)
    -- nodeLength = length $ show node
    -- startZeros = countStart (firstLine) ' '
    -- marginL = div (length (strip firstLine) - nodeLength) 2 + startZeros
    -- marginR = lineLength - nodeLength - marginL
    -- nodeString = (replicate marginL ' ') ++ (show node) ++ (replicate marginR ' ')
    nodeString = evenMargin (show node) lineLength
    connectionsString = drawConnections $ map head strChildren  

-- All of the characters used to represent connections: ┌┬┴┼─┐
-- This helper function assembles all the child lines for a given parent
drawConnections :: [String] -> String
drawConnections nodes = case nodes of
  node:[] -> segment
    where
      (marginL, marginR) = getMargins 1 $ length node
      segment = makeSegment marginL marginR ' ' '|' ' '
  _:_ -> replace line mid c
    where
      line = getSegments (map length nodes) True
      mid = div (length line) 2
      c = case line !! mid of
        '┬' -> '┼'
        _   -> '┴'
  _ -> ""

-- A simple thing that replaces an element in array
replace :: [a] -> Int -> a -> [a]
replace l i x = (take i l) ++ x : (drop (i+1) l)

--draws the lines from children
getSegments :: [Int] -> Bool -> String
getSegments lengths isFirst = case lengths of
  [] -> ""
  x:xs -> segment ++ (getSegments xs False)
    where
      (marginL, marginR) = getMargins 1 x
      segment
        | length xs == 0 = makeSegment marginL marginR '─' '┐' ' '
        | isFirst = (makeSegment marginL marginR ' ' '┌' '─') ++ "─"
        | otherwise = (makeSegment marginL marginR '─' '┬' '─') ++ "─"

-- This helper function repeats charaters 1 and 3 a given amount of times
-- and shoves character 2 between them
makeSegment :: Int -> Int -> Char -> Char -> Char -> String
makeSegment marginL marginR c1 c2 c3 =
  (replicate marginL c1) ++ c2 : (replicate marginR c3)

-- Nevermind...
-- strip :: String -> String
-- strip s = (stripL.reverse) $ (stripL.reverse) s

-- stripL :: String -> String
-- stripL s = case s of
--   "" -> ""
--   c:cs -> case c of
--     ' ' -> stripL cs
--     _   -> s
-- 
-- countStart :: String -> Char -> Int
-- countStart s c = case s of
--   "" -> 0
--   x:xs
--     | x == c -> 1 + countStart xs c
--     | otherwise -> 0



-- The remaining functions all deal with taking lines that represent
-- children of a node and merging those lines into a single picture
-- while keeping the spacing between the cihldren


mergeNodes :: [[String]] -> [String]
mergeNodes ll = mergeTreeLevels $ makeEvenDepth ll

mergeTreeLevels :: [[String]] -> [String]
mergeTreeLevels ll
  | heads == "" = []
  | otherwise = heads : (mergeTreeLevels tails)
    where (heads, tails) = headsAndTails ll

headsAndTails :: [[String]] -> (String, [[String]])
headsAndTails ll = case ll of
  l:ls -> case l of
    [] -> (heads, tails)
    _  -> case heads of
      "" -> ((head l), (tail l):tails)
      _  -> ((head l) ++ ' ' : heads, (tail l):tails)
    where (heads, tails) = headsAndTails ls
  [] -> ("", [])

makeEvenDepth :: [[String]] -> [[String]]
makeEvenDepth layers = map (\l -> adjustLines l) layers
  where
    maxLength :: [[a]] -> Int
    maxLength ll = case ll of
      [] -> 0
      _  -> maximum $ map length ll

    maxLine :: [String] -> String
    maxLine l = case l of
      [] -> ""
      _  -> replicate (maxLength l) ' '

    maxDepth = maxLength layers

    adjustLines :: [String] -> [String]
    adjustLines layer = sameLenLines ++ remainingLines
      where
        maxLineLength = length $ maxLine layer
        sameLenLines = map (\s -> evenMargin s maxLineLength) layer
        remainingLines = (replicate (maxDepth - length layer) (maxLine layer))

-- This functions positions a string with center alignment
evenMargin :: String -> Int -> String
evenMargin s targetLength =
  let (marginL, marginR) = getMargins (length s) targetLength
  in (replicate marginL ' ') ++ s ++ (replicate marginR ' ')

-- A helper function for evenMargin
getMargins :: Int -> Int -> (Int, Int)
getMargins sLength targetLength = (marginL, marginR)
  where
    marginL = div (targetLength - sLength + 1) 2
    marginR = targetLength - sLength - marginL

