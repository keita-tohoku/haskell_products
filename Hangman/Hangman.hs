import Data.Char
import System.Random
import HangmanWords
import Data.List

main :: IO()
main = do
   n <- (getStdRandom $ randomR (0,(length hangmanWords)-1) :: IO Int)  
   loop (makeInitState $ hangmanWords !! n)


data HangmanState =
     H           --constructer
     String      --Secret word
     [Char]      --tried chars
     Integer     --life
   deriving Show

loop state =
     if isHangmanFinished state then
     	reportFinished state
     else do
     	reportState state
	c <- getUserInput
	if checkInputChar c
	then do
	    let state' = updateHangmanState c state
	    loop state'
	else do
	    putStrLn "Oh, your put is not alphabet."
	    putStrLn "Please put A-Z."
	    loop state
	    

checkInputChar :: Char -> Bool
checkInputChar c =
    if c >= 'A' && c <= 'Z'
    then True
    else False


makeInitState :: String -> HangmanState
makeInitState secretWord = (H secretWord [] 5)
	      

isHangmanFinished :: HangmanState -> Bool
isHangmanFinished (H s cs n) =
    if n == -1 then
       True
    else if checkSecret s cs then
       True
    else
       False

checkSecret :: String -> String -> Bool
checkSecret [] cs = True
checkSecret (x:sw) cs =
    if elem x cs then
       checkSecret sw cs
    else False


reportFinished :: HangmanState -> IO()
reportFinished (H s cs l) =
    if checkSecret s cs 
    then do
        putStrLn "--------------------------"
	putStrLn "Game Clear!"
	putStrLn ("The secret word is " ++ s)
	return ()
    else do
        putStrLn "--------------------------"
	putStrLn "Game Over..."
	putStrLn ("The secret word is " ++ s)
	return ()


reportState :: HangmanState -> IO()
reportState (H s cs l) = do
    putStrLn "--------------------------"
    putStr "The secret word is "
    secretOutput s cs
    putStrLn ("You played      -> " ++ cs)
    putStrLn ("Your Life is " ++ (show l))
    putStrLn "↓ Put Character "

secretOutput :: String -> [Char] -> IO()
secretOutput [] words = putStrLn ""
secretOutput (x:xs) words =
    if elem x words then do
        putChar x
	secretOutput xs words
    else do
    	putStr "?"
	secretOutput xs words


getUserInput :: IO Char
getUserInput = do
    s <- getLine
    if null s then getUserInput
    else do
        let c = toUpper $ head $ s
        return c


updateHangmanState :: Char -> HangmanState -> HangmanState
updateHangmanState c (H s cs l) =
    if elem c cs then
       (H s cs l)
    else if elem c s then
       (H s (c:cs) l)
    else
       (H s (c:cs) (l-1))

