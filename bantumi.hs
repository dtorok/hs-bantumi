import Debug.Trace

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
moveStones pos board@Board{holes=holes} = distribute (pos+1) value board{holes=emptyAt pos holes}
	where
		value = holes `at` pos

		distribute :: Int -> Int -> Board -> Board
		distribute _ 0 board = board
		distribute p v board | p >= ( 2 * size + 2) = distribute 0 v board
		distribute p v board@Board{holes=holes} =  distribute (p + 1) (v - 1) board{holes=hs'} where
			hs' = modifyAt (+1) p holes
			changePlayerIfNeeded board | and [v == 1, isColl p] = board
			changePlayerIfNeeded board | and [v == 1, (stonesAt p board) /= 1] = changePlayer board
			changePlayerIfNeeded board = board

		other :: Int -> Int
		other 0 = 1
		other 1 = 0
		other _ = undefined

normalizePos :: Int -> Int
normalizePos pos = pos `mod` (2 * (size + 1))

opositePos :: Int -> Int
opositePos pos | pos < size = size + (size - pos)
opositePos pos | pos > size = size * 2 - pos -- size - 1 - pos + size + 1

changePlayer :: Board -> Board
changePlayer board@Board{player=player} = board{player=player'}
	where
		player' = case player of
			0 -> 1
			_ -> 0

stonesAt :: Int -> Board -> Int
stonesAt pos Board{holes=holes} = holes `at` pos

holeBoundsFor :: Int -> (Int, Int)
holeBoundsFor pl = (offset, offset + size - 1)
	where
		offset = pl * (size + 1)

holesFor :: Int -> Board -> [Int]
holesFor pl board = (take $ hlast - hfirst + 1) . (drop hfirst) . holes $ board
	where
		(hfirst, hlast) = holeBoundsFor pl

allHolesEmpty :: Int -> Board -> Bool
allHolesEmpty pl board = sumOfHoles == 0
	where
		sumOfHoles = sum . (holesFor pl) $ board

cleanupAllHoles :: Int -> Board -> Board
cleanupAllHoles pl board = foldl cleanupAt board [hfirst..hlast]
	where
		cleanupAt board pos = collectStones pl pos board
		(hfirst, hlast) = holeBoundsFor pl

collectStones :: Int -> Int -> Board -> Board
collectStones pl pos board = board{holes=holes'} where
	holes' = addToColl . emptyPos . holes $ board

	emptyPos = modifyAt (\ _ -> 0) pos
	addToColl = modifyAt (+ stones) collPos

	stones = stonesAt pos board
	collPos = pl * (size + 1) + size

emptyAt :: Int -> [Int] -> [Int]
emptyAt = modifyAt (\ _ -> 0)

modifyAt :: (Int -> Int) -> Int -> [Int] -> [Int]
modifyAt _ _ [] = undefined
modifyAt f 0 (x:xs) = ((f x):xs)
modifyAt f p (x:xs) = (x:(modifyAt f (p-1) xs))

isColl :: Int -> Bool
isColl pos = or [pos == size, pos == 2 * size + 1]

belongsToPlayer :: Int -> Int -> Bool
belongsToPlayer pos player = and [pos >= offset, pos < offset + size] where
	offset = player * (size + 1)


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


makeStep :: Int -> Board -> (Int, Board)
makeStep pos board = (makeResult . changePlayerRule . cleanupAtTheEnd . stealStonesRule . moveStonesRule) board
	where
		lasthole = normalizePos $ pos + (stonesAt pos board)

		moveStonesRule board' = moveStones pos board'

		stealStonesRule board' = if and [playersHole, lastWasEmpty] then newboard else board' where
			pl = player board'
			playersHole = belongsToPlayer lasthole (player board)
			lastWasEmpty = stonesAt lasthole board' == 1
			newboard = (collectStones pl $ opositePos lasthole) . 
			           (collectStones pl lasthole) $ board'

		changePlayerRule board' = case isColl lasthole of
										True -> board'
										False -> changePlayer board'

		cleanupAtTheEnd board' = if or [allHolesEmpty 0 board', allHolesEmpty 1 board'] 
								 then (cleanupAllHoles 0) . (cleanupAllHoles 1) $ board'
								 else board'


		makeResult :: Board -> (Int, Board)
		makeResult board' = (winner, board')
			where
				winner = case and [allHolesEmpty 0 board', allHolesEmpty 1 board'] of
							False -> -1
							True -> if scoreP1 > scoreP2 
									then 0 
									else if scoreP1 < scoreP2 
										 then 1
										 else 2
								where (scoreP1, scoreP2) = getState board'


gameLoop :: Board -> IO ()
gameLoop board = do
		putStr "\n"
		printBoard board
		putStr $ "Next player: player" ++ (show $ (player board) + 1) ++ "\n"

		ch <- getInput

		let (winner, newboard) = case validateInput ch board of
			Nothing -> (-1, board)
			Just pos -> makeStep pos board

		case winner of
			-1 -> gameLoop newboard
			2  -> do
				putStr "\n\n*****\n\n"
				printBoard newboard
				print $ "DRAW!"
			_  -> do
				putStr "\n\n*****\n\n"
				printBoard newboard
				print $ "THE WINNER IS PLAYER" ++ (show $ winner+1)


test :: IO ()
test = do
		_ <- test "validateInput" testValidateInput
		_ <- test "isColl" testIsColl
		_ <- test "normalizePos" testNormalizePos
		_ <- test "belongsToPlayer" testBelongsToPlayer
		_ <- test "opositePos" testOpositePos
		_ <- test "holeBoundsFor" testHoleBoundsFor
		putStr "Done.\n"
	where
		test :: String -> IO () -> IO ()
		test title testfunction = do
			putStr $ "Testing: " ++ title
			testfunction
			putStr "\n"

		testValidateInput :: IO ()
		testValidateInput = do
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

		testIsColl :: IO ()
		testIsColl = do
			assertEquals (isColl $ 0)            False
			assertEquals (isColl $ size - 1)     False
			assertEquals (isColl $ size)         True
			assertEquals (isColl $ size + 1)     False
			assertEquals (isColl $ 2 * size)     False
			assertEquals (isColl $ 2 * size + 1) True

		testNormalizePos :: IO ()
		testNormalizePos = do
			let fullsize = 2 * (size + 1)
			assertEquals (normalizePos 0) 0
			assertEquals (normalizePos 1) 1
			assertEquals (normalizePos size) size
			assertEquals (normalizePos $ fullsize - 1) (fullsize - 1)
			assertEquals (normalizePos fullsize) 0
			assertEquals (normalizePos $ fullsize + 1) 1

		testBelongsToPlayer :: IO ()
		testBelongsToPlayer = do
			assertEquals (belongsToPlayer 0 0)               True
			assertEquals (belongsToPlayer 1 0)               True
			assertEquals (belongsToPlayer (size-1) 0)        True
			assertEquals (belongsToPlayer  size    0)        False
			assertEquals (belongsToPlayer (size+1) 0)        False
			assertEquals (belongsToPlayer (size+1) 1)        True
			assertEquals (belongsToPlayer (size+1+size-1) 1) True
			assertEquals (belongsToPlayer (size+1+size) 1)   False

		testOpositePos :: IO ()
		testOpositePos = do
			assertEquals (opositePos 0) (size + size)
			assertEquals (opositePos $ size - 1) (size + 1)
			assertEquals (opositePos $ size + 1) (size - 1)
			assertEquals (opositePos $ size + size) 0

		testHoleBoundsFor :: IO ()
		testHoleBoundsFor = do
			assertEquals (holeBoundsFor 0) (0, size - 1)
			assertEquals (holeBoundsFor 1) (size + 1, size + size)

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



