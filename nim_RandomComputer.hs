import System.Random
type Board = [Int]

board :: Board
board = [4,3,7]

displayBoard :: Board -> IO ()
displayBoard board = do
	mapM_ (putStrLn) $ map (\x -> replicate x '|') board

validMove :: Int -> Int -> Board -> Bool
validMove row number board = validRow && hasSticks && (not emptyRow)
	where emptyRow = (board !! row == 0)
	      validRow = (row < (length board))
	      hasSticks = (board !! row) >= number


removeSticks :: Int -> Int -> Board -> Board
removeSticks _ _ [] = []
removeSticks row number (x:xs)
	| row == 0 = (x - number):(removeSticks (row - 1) number xs)
	| otherwise = x:(removeSticks (row - 1) number xs)

winner :: Board -> Bool
winner board = sum( board ) == 0

random_Computer board = do
	gen <- getStdGen
	let row = (randomRs (1,3) gen)
	if(board!!row /= 0)
		then do
			let num = (randomRs (1,board!!row) gen)
			let new_board = remove_sticks (read row) (read num) board
			return new_board
		else do
			let new_board = random_Computer board
			return new_board

game :: Board -> IO()
game board = do
	displayBoard board
	
	putStr "Enter Row: "
	row <- getLine
	putStr "Enter Sticks to take: "
	number <- getLine

	let valid =  validMove (read row) (read number) board
	if (valid)
		then do putStrLn "Valid Move!";
			let new_board = removeSticks (read row) (read number) board
			let won = winner (new_board)
			if(won)
				then do putStrLn "Human won!"
				else do 
			-- Make computer move
			game new_board
			
		else do 
			putStrLn "Please enter a valid move.";
			game board
	


playGame :: IO()
playGame = do
	putStrLn "Welcome to NIM!"
	game board
	


