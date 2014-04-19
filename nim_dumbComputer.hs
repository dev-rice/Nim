import Control.Concurrent

type Board = [Int]

board :: Board
board = [4,3,7]

displayBoard :: Board -> IO ()
displayBoard board = do
	mapM_ (putStrLn) $ map (\x -> replicate x 'X') board
	putStrLn ""

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

dumb_computer :: Board -> Board
dumb_computer (x:xs)
	| x /= 0 = removeSticks 0 x (x:xs)
	| otherwise = x:(dumb_computer xs)

let_computer_think :: IO()
let_computer_think = do
	putStr "Computer thinking ."
	threadDelay(500000)
	putStr " ."
	threadDelay(500000)
	putStr " ."
	threadDelay(500000)
	putStrLn ""

game :: Board -> IO()
game board = do
	displayBoard board
	
	putStr "Enter Row: "
	row <- getLine
	putStr "Enter Sticks to take: "
	number <- getLine

	let valid =  validMove (read row) (read number) board
	if (valid)
		then do 
			let new_board = removeSticks (read row) (read number) board
			let won = winner (new_board)
			
			if(won)
				then do putStrLn "Human won!"
				else do 

			displayBoard new_board

			-- Make computer move
			let end_board = dumb_computer new_board
			let won = winner (end_board)
			
			let_computer_think

			if(won)
				then do putStrLn "Computer won!"
				else do 

			game end_board
			
		else do 
			putStrLn "Please enter a valid move.";
			game board
	


playGame :: IO()
playGame = do
	putStrLn "Welcome to NIM!"
	game board
	


