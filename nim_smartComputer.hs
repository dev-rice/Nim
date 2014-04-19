import Data.Bits
import Data.List

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

nim_sum :: Board -> Int
nim_sum board = foldr1 (\acc x -> xor acc x) board

msb :: Int -> Int
msb number = floor $ logBase 2 (fromIntegral number)

find_xk :: Int -> Board -> Int
find_xk s board
	| s > maximum || num == 0 = maximum
	| s /= 1 = num
	| otherwise = head $ filter (odd) board
	where    maximum = foldr1 (max) board
		 num = head $ filter (\x -> mod x s == 0) board

smart_computer :: Board -> Board
smart_computer board = removeSticks row to_remove board
	where     s = nim_sum board
		  xk = find_xk s board
		  yk = xor s xk
		  to_remove = xk - yk
		  row = head $ elemIndices xk board 

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

			-- Make computer move
			displayBoard new_board
			let end_board = smart_computer new_board
			let won = winner (end_board)
			
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
	


