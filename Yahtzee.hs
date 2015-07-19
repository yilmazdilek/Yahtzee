module Yahtzee where

input :: [[Int]]
input =	[
		[1,2,3,4,5],
		[1,2,3,4,5],
		[1,2,3,4,5],
		[1,2,3,4,5],
		[1,2,3,4,5],
		[1,2,3,4,5],
		[1,2,3,4,5],
		[1,2,3,4,5],
		[1,2,3,4,5],
		[1,2,3,4,5],
		[1,2,3,4,5],
		[1,2,3,4,5],
		[1,2,3,4,5]
		]

{--
getIntList :: IO [Int]
getIntList = do line <- getLine
                return (read line :: [Int])

typeCast :: Int -> [[Int]] -> IO ()
typeCast n mat = do
					xs <- getIntList
					if (n > 1)
						then typeCast (n-1) (mat ++ [xs])
						else print "done"
--}

sortedInput :: [[Int]] -> [[Int]] -> [[Int]]
sortedInput mat ret
	| null mat	= ret
	| otherwise	= sortedInput (tail mat) (ret ++ [quickSort (head mat)])
		where
			quickSort :: [Int] -> [Int]
			quickSort []			= []
			quickSort (pivot:xs)	= quickSort [x | x <- xs, x <= pivot]
									++ [pivot]
									++ quickSort [x | x <- xs, x > pivot]

constructMatrix :: Int -> [[Int]] -> [[Int]]
constructMatrix x mat
	| x == 13	= mat
	| otherwise	= constructMatrix (x+1) 
					(mat ++ [[values ((sortedInput input []) !! x) 0 1,
							values ((sortedInput input []) !! x) 0 2,
							values ((sortedInput input []) !! x) 0 3,
							values ((sortedInput input []) !! x) 0 4,
							values ((sortedInput input []) !! x) 0 5,
							values ((sortedInput input []) !! x) 0 6,
							chance ((sortedInput input []) !! x),
							kinds ((sortedInput input []) !! x) ((sortedInput input []) !! x) 3,
							kinds ((sortedInput input []) !! x) ((sortedInput input []) !! x) 4,
							kinds ((sortedInput input []) !! x) ((sortedInput input []) !! x) 5,
							shortStraight ((sortedInput input []) !! x),
							longStraight ((sortedInput input []) !! x),
							fullHouse ((sortedInput input []) !! x)
							]])
		where
			values :: [Int] -> Int -> Int -> Int
			values xs acc what
				| null xs	= acc * what
				| otherwise	= values (tail xs) (acc + (if (head xs == what) then 1 else 0)) what

			chance :: [Int] -> Int
			chance xs = foldl1 (+) xs

			kinds :: [Int] -> [Int] -> Int -> Int
			kinds ys (x:xs) howMany
				| length (x:xs) < howMany				= 0
				| length (filter (==x) ys) >= howMany	= if (howMany == 5) then 50 else chance ys
				| otherwise								= kinds ys (filter (/=x) xs) howMany

			shortStraight :: [Int] -> Int
			shortStraight xs
				| longStraight xs == 35	= 25
				| length xs == 5		= shortStraight(tail xs)
										+ shortStraight(reverse(tail(reverse xs)))
				| length xs == 4		= if (isSequence xs) then 25 else 0
				| otherwise				= 0
				
			longStraight :: [Int] -> Int
			longStraight xs
				| length xs == 5	= if (isSequence xs) then 35 else 0
				| otherwise			= 0
				
			isSequence :: [Int] -> Bool
			isSequence xs
				| length xs == 1	= True
				| otherwise			= if ((head xs) + 1 == head (tail xs))
										then isSequence (tail xs)
										else False

			fullHouse :: [Int] -> Int
			fullHouse xs
				| length (filter (==(head xs)) xs) == 3	= if (length filtered == 2) then 40 else 0
				| length (filter (==(head xs)) xs) == 2	= if (length filtered == 3) then 40 else 0
				| otherwise								= 0 
					where
						others = filter (/=(head xs)) xs
						filtered = filter (==(head others)) others												

type Coordinates	= (Int, Int)
type PointTaken		= (Int, Coordinates)

getPoints :: [[Int]] -> [PointTaken] -> [PointTaken]
getPoints mat out
	| length out == length mat	= out
	| otherwise					= getPoints (extractMaxRowCol mat [] 0 0 maxRow maxCol) 
											(out ++ [findMax mat (-1) 0 0 0 0])
		where
			maxRow = fst (snd (findMax mat (-1) 0 0 0 0))
			maxCol = snd (snd (findMax mat (-1) 0 0 0 0))

			findMax :: [[Int]] -> Int -> Int -> Int -> Int -> Int -> PointTaken
			findMax mat max x y maxRow maxCol
				| x == length mat	= (max, (maxRow, maxCol))
				| y == length mat	= findMax mat max (x+1) 0 maxRow maxCol
				| otherwise			= if(((mat !! x) !! y) > max)
										then findMax mat ((mat !! x) !! y) x (y+1) x y
										else findMax mat max x (y+1) maxRow maxCol
			
			extractMaxRowCol :: [[Int]] -> [[Int]] -> Int -> Int -> Int -> Int -> [[Int]]
			extractMaxRowCol mat ret x y maxRow maxCol
				| x == length mat				= reverse (tail (reverse ret))
				| y == length mat				= extractMaxRowCol mat (ret ++ [[]]) (x+1) 0 maxRow maxCol
				| y == maxCol || x == maxRow	= extractMaxRowCol mat (attach ret (-1)) x (y+1) maxRow maxCol
				| otherwise						= extractMaxRowCol mat (attach ret ((mat !! x) !! y)) 
																	x (y+1) maxRow maxCol
					where
						attach :: [[Int]] -> Int -> [[Int]]
						attach mat val
							| null mat	= [[val]]
							| otherwise	= reverse (tail (reverse mat)) ++ [head (reverse mat) ++ [val]]

getResults :: [Int]
getResults = splitPoints (pointSort (getPoints (constructMatrix 0 []) [])) []
	where
		splitPoints :: [PointTaken] -> [Int] -> [Int]
		splitPoints ps ret
			| null ps	= ret
			| otherwise	= splitPoints (tail ps) (ret ++ [fst (head ps)])

		pointSort :: [PointTaken] -> [PointTaken]
		pointSort []			= []
		pointSort (pivot:xs)	= pointSort [x | x <- xs, snd (snd x) <= snd (snd pivot)]
								++ [pivot]
								++ pointSort [x | x <- xs, snd (snd x) > snd (snd pivot)]

attachBonusAndTotal :: [Int] -> Int -> Int -> Int -> [Int]
attachBonusAndTotal ps bonus total x
	| x > 12	= ps ++ [total]
	| x == 6		= attachBonusAndTotal (if (bonus >= 63) then ps ++ [35] else ps ++ [0])
															bonus (total + (ps !! x)) (x+1)
	| otherwise	= attachBonusAndTotal ps (bonus + (ps !! x)) (total + (ps !! x)) (x+1)

blg458EProject :: [Int]
blg458EProject = attachBonusAndTotal getResults 0 0 0
