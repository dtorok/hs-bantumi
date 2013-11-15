size :: Int
size = 6

starter_stones :: Int
starter_stones = 4

data Board = Board {
	player :: Int,
	holes :: [Int]
} deriving (Show)

createBoard :: Board
createBoard = Board { 
		player = 0,
		holes = holes ++ coll ++ holes ++ coll
	} 
	where
		coll = [0]
		holes = take size $ repeat starter_stones

printBoard :: Board -> IO()
printBoard Board{holes=holes} = do
		print p2holes 
		print colls
		print p1holes 
	where
		sholes = map show holes
		p1holes = unwords $ [" "] ++ (take size sholes)
		p2holes = unwords $ [" "] ++ (reverse $ take size $ drop (size+1) sholes)
		colls = unwords $ [(head . (drop (2*size+1))) sholes] ++ (take size $ repeat " ") ++ [(head . (drop size)) sholes]

getState :: Board -> (Int, Int)
getState board = (stonesAt size board, stonesAt (size * 2 + 1) board)

moveStones :: Int -> Board -> Board
moveStones pos board@Board{holes=holes} = board{holes=(distribute (pos+1) value) . (emptyAt pos) $ holes}
	where
		value = holes `at` pos

		distribute :: Int -> Int -> [Int] -> [Int]
		distribute _ 0 hs = hs
		distribute p v hs | p >= ( 2 * size + 2) = distribute 0 v hs
		distribute p v hs = distribute (p + 1) (v - 1) hs' where
			hs' = modifyAt (+1) p hs
		
		emptyAt = modifyAt (\ _ -> 0)

		other :: Int -> Int
		other 0 = 1
		other 1 = 0
		other _ = undefined		

changePlayer :: Board -> Board
changePlayer board@Board{player=player} = board{player=player'}
	where
		player' = case player of
			0 -> 1
			_ -> 0

stonesAt :: Int -> Board -> Int
stonesAt pos Board{holes=holes} = holes `at` pos

modifyAt :: (Int -> Int) -> Int -> [Int] -> [Int]
modifyAt _ _ [] = undefined
modifyAt f 0 (x:xs) = ((f x):xs)
modifyAt f p (x:xs) = (x:(modifyAt f (p-1) xs))

isColl :: Int -> Bool
isColl pos = or [pos == size, pos == 2 * size + 1]


at :: [a] -> Int -> a
at [] _ = undefined
at (x:xs) 0 = x
at (x:xs) pos = xs `at` (pos - 1)

getInput :: IO Char
getInput = getChar

validateInput :: Char -> Board -> Maybe Int
validateInput ch board@Board{player=player} = case and [numberChosen, ownHoleChosen, holeHasStones] of
									True -> Just realPos
									False -> Nothing
	where
		numberChosen = and [ch >= '0', ch <= '9']
		offset = player * (size + 1)
		ownPos = read (ch:[]) :: Int
		realPos = ownPos + offset
		ownHoleChosen = and [ownPos >= 0, ownPos < size]
		holeHasStones = stonesAt realPos board > 0


gameLoop :: Board -> IO ()
gameLoop board = do
		putStr "\n"
		printBoard board

		ch <- getInput

		let board' = case validateInput ch board of
			Nothing -> board
			Just pos -> moveStones pos board

		gameLoop board'

test :: IO ()
test = do
		_ <- testValidateInput
		_ <- testIsColl
		putStr "Done.\n"
	where
		testValidateInput :: IO ()
		testValidateInput = do
			putStr "Testing: validateInput"
			let board1 = ((moveStones 11) . (moveStones 5) . (moveStones 4)) createBoard
			let board2 = changePlayer board1
			assertEquals (validateInput '0' board1) (Just 0)
			assertEquals (validateInput '1' board1) (Just 1)
			assertEquals (validateInput '6' board1) (Nothing) -- overflow
			assertEquals (validateInput '7' board1) (Nothing) -- overflow
			assertEquals (validateInput 'a' board1) (Nothing) -- invalid
			assertEquals (validateInput '0' board2) (Just 7)
			assertEquals (validateInput '4' board2) (Nothing) -- empty
			assertEquals (validateInput '6' board2) (Nothing) -- overflow
			putStr "\n"

		testIsColl :: IO ()
		testIsColl = do
			putStr "Testing: isColl"
			assertEquals (isColl $ 0)            False
			assertEquals (isColl $ size - 1)     False
			assertEquals (isColl $ size)         True
			assertEquals (isColl $ size + 1)     False
			assertEquals (isColl $ 2 * size)     False
			assertEquals (isColl $ 2 * size + 1) True
			putStr "\n"

		assertEquals :: (Show a, Eq a) => a -> a -> IO ()
		assertEquals x y | x == y = putStr "."
		assertEquals x y = error ("\nAssert: " ++ (show x) ++ " != " ++ (show y))


main :: IO ()
main = do
	gameLoop createBoard
	--let board = createBoard
	--let board' = moveStones 1 board
	--let board'' = moveStones 0 board'
	--let board''' = moveStones 5 board''
	--let board'''' = moveStones 10 board'''
	--printBoard board
	--printBoard board'
	--printBoard board''
	--printBoard board'''
	--print $ getState board'''
	--printBoard board''''
	--print $ getState board''''



