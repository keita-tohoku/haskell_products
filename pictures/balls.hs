module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import qualified Data.Map as M
import Data.Map ((!))
import Control.Applicative
import System.Random

data Ball = Ball { bax :: Float,
                   bay :: Float,
                   ver :: Point }

data Block = Block { lives :: Int,
                     blx :: Float,
                     bly :: Float }

type Blocks = [Block]

data State = State { block :: Blocks,
                     ball :: Ball,
                     life :: Int }

initWorld :: State
initWorld = State { block = blockMaker,
                    ball = Ball { bax = 0,
                                  bay = (-250),
                                  ver = (0,0) },
                    life = 10}


blockMaker :: Blocks
blockMaker = [Block {lives = 4,
                     blx =   0,
                     bly =  50},
	      Block {lives = 5,
 	      	     blx =  75,
		     bly = 100},
	      Block {lives = 2,
	      	     blx = 100,
		     bly =  25},
	      Block {lives = 4,
	      	     blx = -100,
		     bly = 175},
              Block {lives = 3,
	      	     blx = -25,
		     bly = 175},
	      Block {lives = 2,
	             blx = -75,
		     bly = -25}]


drawWorld :: State -> Picture
drawWorld s@State{block = [], ball = Ball{ bax = bx,bay = -250, ver = v},life = l} =  gameclear
drawWorld s@State{block = blos, ball = Ball{ bax = bx,bay = -250, ver = (0,0)},life = 0} =  gameover
drawWorld s = pictures ((drawLives s) : drawEmptyBox : ((drawBall (ball s)) : [drawBlock bl | bl <- block s])) 


gameclear :: Picture
gameclear = color black $ translate (-175) (-25) $ scale 0.5 0.5 $ text ("Game Clear!")


gameover :: Picture
gameover = color black $ translate (-175) (-25) $ scale 0.5 0.5 $ text ("Game Over...")


drawLives :: State -> Picture
drawLives s = translate (-170) (254) $ scale 0.2 0.2 $ color black $ text ("Your Life is " ++ (show (life s))) 


drawEmptyBox :: Picture
drawEmptyBox = color black $ line[(-175,-250),(175,-250),(175,250),(-175,250),(-175,-250)]


drawBall :: Ball -> Picture
drawBall ball = translate (bax ball) (bay ball) $ color red $  circleSolid 5


drawBlock :: Block -> Picture
drawBlock blo  =
    let l = lives blo
        bx = blx blo
	by = bly blo
   in if l == 3 then translate bx by $ color yellow $ rectangleSolid 50 50
      else if l == 5 then translate bx by $ color black $ rectangleSolid 50 50
      else if l == 4 then translate bx by $ color blue $ rectangleSolid 50 50
      else if l == 2 then translate bx by $ color orange $ rectangleSolid 50 50
      else translate bx by $ color red $ rectangleSolid 50 50


verAbs = 200
mycos :: Point -> Point -> Float
mycos (x1,y1) (x2,y2) = (x2 - x1) / sqrt ((x2-x1)^2 + (y2-y1)^2)
mysin :: Point -> Point -> Float
mysin (x1,y1) (x2,y2) = (y2 - y1) / sqrt ((x2-x1)^2 + (y2-y1)^2)


eventHandler :: Event -> State -> State
eventHandler (EventKey (MouseButton LeftButton) Up _ (mx,my) ) s@State{block = blos, ball = Ball{ bax = bx,bay = -250, ver = v},life = 0} = State{ block = blos,
            ball = Ball{ bax = bx,
                         bay = -250,
                         ver = (0,0) },
            life = 0}
eventHandler (EventKey (MouseButton LeftButton) Up _ (mx,my) ) s@State{block = [], ball = Ball{ bax = bx,bay = -250, ver = v},life = l} = s
eventHandler (EventKey (MouseButton LeftButton) Up _ (mx,my) ) s@State{block = blos, ball = Ball{ bax = bx,bay = -250, ver = v},life = l} =
 let verx = (mycos (bx,(-250)) (mx,my)) * verAbs
     very = (mysin (bx,(-250)) (mx,my)) * verAbs
     newver = (verx , very)
  in State{ block = blos,
            ball = Ball{ bax = bx,
                         bay = -250,
                         ver = newver },
            life = (l-1)}
eventHandler _ s = s


isBlockHit :: State -> Bool
isBlockHit s =
   let blos = block s
       b    = ball s
       bx   = bax b
       by   = bay b
   in any (bbcheck bx by) blos

bbcheck :: Float -> Float -> Block -> Bool
bbcheck bx by blo =
    let blox = blx blo
    	bloy = bly blo
    in (blox -25) <= bx && (blox + 25) >= bx && (bloy - 25) <= by && (bloy + 25) >= by 


updateBalls :: State -> (Ball -> Ball) -> State
updateBalls s f =
    let b = ball s
        bx = bax b
	by = bay b
	(vx,vy) = ver b
    in if isBlockHit s then let newstate = boundState s
       	  	       	    	newblos = block newstate
				newball = ball newstate
			    in s{ block = newblos, ball = f newball}	
       else if bx > 175.0 then s{ ball = f (b{ver = (-vx,vy)})}
       else if bx < (-175.0) then  s { ball = f (b{ver = (-vx,vy)})}
       else if by > 250 then  s { ball = f (b{ver = (vx,-vy)})}
       else if by < (-251) then  s { ball = f (b{bay = (-250),ver = (0,0)})}
       else  s { ball = f b}


boundState :: State -> State
boundState s =
    let blos = block s
    	b    = ball s
	bx   = bax b
	by   = bay b
	n    = (length blos) - (blocheck blos bx by)
	hit_blo = blos !! (n - 1)
    in s { block = new_blos (n-1) blos, ball = new_ball (n-1) b blos}


new_blos :: Int -> Blocks -> Blocks
new_blos n (blo:res) = if n == 0 then let l = lives blo
					  b = blo { lives = l - 1 }
	   	       	       	      in if l - 1 == 0 then res
				         else b : res
		       else blo : (new_blos (n-1) res)


new_ball :: Int -> Ball -> Blocks -> Ball
new_ball n b blos =
    let blo = blos !! n
        bx = bax b
	by = bay b
	blox = blx blo
	bloy = bly blo
	(vx,vy) = ver b
    in if  (by > bloy - 25) && (by < bloy - 20) then b { ver = (vx,-vy), bay = by - (by - (bloy - 25))*2 }
       else if (by < bloy + 25) && (by > bloy + 20) then b { ver = (vx,-vy), bay = by + (bloy + 25 - by) * 2 }
       else if (bx > blox - 25) && (bx < blox - 20) then b { ver = (-vx,vy), bax = bx - (bx - (blox - 25))*2 }
       else if (bx < blox + 25) && (bx > blox + 20) then b { ver = (-vx,vy), bax = bx + ((blox + 25) - bx)*2 }
       else b


blocheck :: Blocks -> Float -> Float -> Int
blocheck (blo:res) bx by =
	 let blox = blx blo
	     bloy = bly blo
	 in if (blox -25) <= bx && (blox + 25) >= bx && (bloy - 25) <= by && (bloy + 25) >= by then length res
	 else blocheck res bx by


tickHandler :: Float -> State -> State 
tickHandler dt s = updateBalls s (\ball -> ball { bax = bax ball + (fst $ ver ball) * dt,
                                               bay = bay ball + (snd $ ver ball) * dt,
                                               ver = ver ball } )


main :: IO()
main = play (InWindow "Balls" (600,600) (20,20))
       white
       60
       initWorld
       drawWorld
       eventHandler
       tickHandler
