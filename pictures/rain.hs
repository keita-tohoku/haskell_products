import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

data World = World { balls :: [Ball]}
data Ball = Ball { bx :: Float
     	    	 , by :: Float
		 , bage :: Float
		 , vel :: Float }


initWorld :: World
initWorld = World [(Ball 0 0 0 5.0)]

drowWorld :: World -> Picture
drowWorld w =
	  let bs = balls w
	  in pictures $ (map (\b -> translate (bx b) (by b) $ color (ballColor (bage b)) $ circleSolid 10) bs)
	  where
	    ballColor :: Float -> Color
	    ballColor t = let p = 1.0 / (1.0 + t)
	    	      	  in mixColors p (1 - p) black white

eventHandler ::  Event -> World -> World
eventHandler (EventKey (MouseButton LeftButton) Down _ (mx,my)) w =
	     let bs = balls w
	         newball = (Ball {bx = mx, by = my, bage = 0, vel = 5.0})
	     in w { balls = (newball : bs) }
eventHandler ev w = w

updateBall :: World -> ([Ball] -> [Ball]) -> World
updateBall w f = w { balls = f (balls w) }

tickHandler :: Float -> World -> World
tickHandler dt w = updateBall w (map (\b -> b { by = by b - (vel b) * dt ,
	       	   	      	       	   bage = bage b + dt ,
					   vel = vel b + 30.0 * dt}))


main :: IO ()
main = play (InWindow "Test" (600,400) (20,20))
       	    white
	    60
	    initWorld
	    drowWorld
	    eventHandler
	    tickHandler