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

type BoxPos = Int
type BoxPoint = (BoxPos , BoxPos)

data Block = Block { lives :: Int,
                     blx :: Float,
                     bly :: Float }

type Blocks = [Block]

data Blocks' = Blocks' { nth :: Int, blos :: Blocks }


data State = State { block :: Blocks,
                     ball :: Ball,
                     life :: Int }


leftEdge = 0 :: BoxPos
rightEdge = 9 :: BoxPos
bottomEdge = 0 :: BoxPos
topEdge = 12 :: BoxPos

boxWidthWorld = 10.0 :: Float
boxHeightWorld = 5.0 :: Float

boxWH = boxWidthWorld / 2
boxHH = boxHeightWorld / 2

boxToWorld :: BoxPoint -> Point
boxToWorld (x,y) = ( fromIntegral x * boxWidthWorld-50,
                     fromIntegral y * boxHeightWorld+boxWH )

okPoint :: BoxPoint -> Bool
okPoint (x,y) = and [ x >= leftEdge, x <= rightEdge,
                      y >= bottomEdge , y <= topEdge ]

pointAddClap :: BoxPoint -> BoxPoint -> BoxPoint
pointAddClap (x1,y1) (x2,y2) = ( clap leftEdge rightEdge (x1 + x2),
                                 clap bottomEdge rightEdge (y1 + y2) )
                               where
                                 clap min max n
                                   | n < min = min
                                   | n > max = max
                                   | otherwise = n
                                                 
initWorld :: State
initWorld = State { block = blockMaker,
                    ball = Ball { bax = 0,
                                  bay = (-250),
                                  ver = (0,0) },
                    life = 5}

blockMaker :: Blocks
blockMaker = [Block {lives = 3,
                     blx = 100,
                     bly = 175},
	      Block {lives = 5,
 	      	     blx = -45,
		     bly = 45}]
--  let n = getStdRandom $ randomR (0,5)
--  in blockmake n

--blockmake :: Int -> [Block]
--blockmake 0 = []
--blockmake n = (Block { lives = randomR (0,3) 1,
                      --blx = 10.0 * (fromIntegral (randomR ((-2),2) 1)),
                      --bly = 10.0  }) : blockmake (n-1)

--blockcheck :: Blocks -> Blocks
--blockcheck [] = []
-- blockcheck (bl:bl') = (bl : blockcheck (filter (check bl) bl'))

-- check :: Block -> Block -> Bool
-- check bl1 bl2 = blx bl1 /= blx bl2 
                

drawWorld :: State -> Picture
drawWorld s = pictures (drawEmptyBox : ((drawBall (ball s)) : [drawBlock bl | bl <- block s]))

drawEmptyBox :: Picture
drawEmptyBox = color black $ line[(-175,-250),(175,-250),(175,250),(-175,250),(-175,-250)]

{-
drawEmptyBox = translate 0 (-250) $ scale 5 5 $ line [(fst ul - boxWH,snd ul + boxWH),(fst bl - boxWH,snd bl - boxWH),
                     (fst br + boxWH,snd br - boxWH),(fst ur + boxWH,snd ur + boxWH),(fst ul - boxWH,snd ul + boxWH) ]
  where ul = boxToWorld (leftEdge,topEdge)
        ur = boxToWorld (rightEdge,topEdge)
        bl = boxToWorld (leftEdge,bottomEdge)
        br = boxToWorld (rightEdge,bottomEdge)
-}

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
eventHandler (EventKey (MouseButton LeftButton) Up _ (mx,my) ) s@State{block = blos, ball = Ball{ bax = bx,bay = -250, ver = v},life = l} =
 let verx = (mycos (bx,(-250)) (mx,my)) * verAbs
     very = (mysin (bx,(-250)) (mx,my)) * verAbs
     newver = (verx , very)
  in State{ block = blos,
            ball = Ball{ bax = bx,
                         bay = -250,
                         ver = newver },
            life = l}
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
    in if (bx > blox - 25) && (bx < blox + 25) then b { ver = (vx,-vy) }
       else b { ver = (-vx,vy) }

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
