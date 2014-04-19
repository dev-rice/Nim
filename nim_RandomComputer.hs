import System.Random
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

random_computer :: Board -> IO ()
random_computer board = do
	row <- randomRIO(0,length(board) - 1)
	let valid_row = board !! row /= 0
	if(valid_row)
		then do
			num <- randomRIO (1, board !! row)
			let new_board = removeSticks row num board

			if (winner(new_board))
				then do putStrLn "Computer won!"
				else do

			game new_board
		else do
			random_computer board

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

			random_computer new_board
			let_computer_think
			
		else do 
			game board
	


playGame :: IO()
playGame = do
	putStrLn "Welcome to NIM!"
	game board
	


