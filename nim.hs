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
removeSticks row number board = board

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
			-- Prompt for and remove n sticks
			-- Make computer move
			-- call game with the modified board
		else do 
			putStrLn "Please enter a valid move.";
			game board
	


playGame :: IO()
playGame = do
	putStrLn "Welcome to NIM!"
	game board
	


