import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Data.Char

import System.Random
import HangmanWords
import Data.List

main :: IO ()
main = do
    n <- (getStdRandom $ randomR (0,(length hangmanWords)-1))
    play (InWindow "Hangman Game" (600,400) (20,20))
       	    black
	    30
	    (initState $ hangmanWords !! n)
	    drowState --print hangman state
	    eventHandler -- the motion when press key
	    tickHandler


data HangmanState =
     H { secret :: String,      --Secret word
         tried   :: [Char],      --tried chars
         life   :: Integer     --life
       }

initState :: String -> HangmanState
initState secretWord = (H secretWord [] 5)

drowState :: HangmanState -> Picture
drowState state =
	  if isHangmanFinished state
	  then reportFinished state
	  else reportState state


isHangmanFinished :: HangmanState -> Bool
isHangmanFinished state =
    if life state == -1 then
       True
    else if checkSecret (secret state) (tried state) then
       True
    else
       False


checkSecret :: String -> String -> Bool
checkSecret [] cs = True
checkSecret (x:sw) cs =
    if elem x cs then
       checkSecret sw cs
    else False


reportFinished :: HangmanState -> Picture
reportFinished state =
	       if checkSecret (secret state) (tried state)
	       then pictures (winstate state ++ (hangman (life state))) 
	       else pictures (losestate state ++ (hangman (life state)))


reportState :: HangmanState -> Picture
reportState state =
	   let text1 = translate (-280) (-90) $ scale 0.3 0.3 $ color red $ text ("The secret word is " ++ secretOutput (secret state) (tried state))
	       text2 = translate (-280) (-130) $ scale 0.3 0.3 $ color red $ text ("You tried : " ++ (tried state))
	       text3 = translate (-280) (-170) $ scale 0.3 0.3 $ color red $ text ("Your Life is " ++ (show (life state)))
	   in pictures ([text1,text2,text3] ++ hangman (life state))


winstate :: HangmanState -> [Picture]
winstate state =
    let text1 = translate (-280) (-90) $ scale 0.3 0.3 $ color red $ text ("The secret word is " ++ secretOutput (secret state) (tried state))
        text2 = translate (-280) (-130) $ scale 0.3 0.3 $ color red $ text ("You tried : " ++ (tried state))
	text3 = translate (-280) (-170) $ scale 0.3 0.3 $ color red $ text ("Your Life is " ++ (show (life state)))
	text0 = translate (-280) (-50) $ scale 0.3 0.3 $ color red $ text ("You win !") in [text0,text1,text2,text3]

losestate :: HangmanState -> [Picture]
losestate state =
	   let text1 = translate (-280) (-90) $ scale 0.3 0.3 $ color red $ text ("The secret word is " ++ (secret state))
	       text2 = translate (-280) (-130) $ scale 0.3 0.3 $ color red $ text ("You tried : " ++ (tried state))
	       text3 = translate (-280) (-170) $ scale 0.3 0.3 $ color red $ text ("Your Life is " ++ (show (life state)))
	       text0 = translate (-280) (-50) $ scale 0.3 0.3 $ color red $ text ("You lose.....") in [text0,text1,text2,text3]

secretOutput :: String -> [Char] -> String
secretOutput [] words =  ""
secretOutput (x:xs) words =
    if elem x words then
        x : (secretOutput xs words)
    else
       '?': (secretOutput xs words)
       

hangman :: Integer -> [Picture]
hangman n =
	    let rope = translate 150 180 $ color red $ line [(0,-20),(0,20)]
	        head = translate 150 140 $ color red $ circleSolid 20
		body = translate 150 80 $ scale 0.5 1.0 $ color red $ circleSolid 40
		leftH = translate 150 40 $ color red $ line [(10,50),(30,120)]
		rightH = translate 150 40 $ color red $ line [(-10,50),(-30,120)]
		leftL = translate 150 110 $ color red $ line [(10,-50),(30,-120)]
		rightL = translate 150 110 $ color red $ line [(-10,-50), (-30,-120)]
	in if n == 5 then [rope] else
	   if n == 4 then [rope,head] else
	   if n == 3 then [rope,head,body] else
	   if n == 2 then [rope,head,body,leftH] else
	   if n == 1 then [rope,head,body,leftH,rightH] else
	   if n == 0 then [rope,head,body,leftH,rightH,leftL] else
	   [rope,head,body,leftH,rightH,leftL,rightL]



eventHandler :: Event -> HangmanState -> HangmanState
eventHandler (EventKey (Char k) Down _ (mx,my)) state =
	     	   	  if checkInputChar (toUpper k)
	     		  then updateHangmanState (toUpper k) state
	    		   else state
eventHandler _ state = state

checkInputChar :: Char -> Bool
checkInputChar c =
    if c >= 'A' && c <= 'Z'
    then True
    else False


updateHangmanState :: Char -> HangmanState -> HangmanState
updateHangmanState c state =
    if elem c (tried state) then
       state
    else if elem c (secret state) then
       H (secret state) (c:(tried state)) (life state)
    else
       H (secret state) (c:(tried state)) ((life state)-1)


tickHandler :: Float -> HangmanState -> HangmanState
tickHandler dt w = w