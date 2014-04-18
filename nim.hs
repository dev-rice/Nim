type Board = [Int]

board :: Board
board = [4,3,7]

displayBoard :: Board -> IO ()
displayBoard board = do
	mapM_ (putStrLn) $ map (\x -> replicate x '|') board

validRow :: Int -> Board -> Bool
validRow row board = (row < (length board)) && (not emptyRow)
	where emptyRow = board !! row == 0

removeSticks :: Int -> Int -> Board -> Board
removeSticks row number board = board


game :: Board -> IO()
game board = do
	displayBoard board
	putStr "Enter Row: "
	row <- getLine
	let valid =  validRow (read row) board
	if (valid)
		then do putStrLn "Valid Row";
		else do 
			putStrLn "Please enter a valid row.";
			game board
	-- Make computer move


playGame :: IO()
playGame = do
	putStrLn "Welcome to NIM!"
	game board
	


