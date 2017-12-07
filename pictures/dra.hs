import Graphics.Gloss
import Data.Monoid

main :: IO ()
main = display (InWindow "Picture test" (400,400) (20,20))
       white
       picture

picture :: Picture
picture =
	  let weye = circle 18
	      weye2 = color white $ circleSolid 18
	      beye = color black $ circleSolid 8
	      nouse = color red $ circleSolid 10
	      mouse = circleSolid 40
	      zinchu = line [(0,-6),(0,20)]
	      dammy1 = color white $ rectangleSolid 200 100
	      dammy2 = color white $ rectangleSolid 100 40
	      bl = circleSolid 92
	      wh = circleSolid 70 in
	      pictures ([color blue $ bl,translate 0 (-10) $ color white $ wh,
	      	         translate 0 (-85) dammy1,
			 color red $ mouse,translate 0 20 $ dammy2,
			 color black $ zinchu, translate 0 20 nouse,
			 translate (-18) 47 $ weye2,
			 translate (-18) 47 $ weye,translate (-10) 47 $ beye,
			 translate 18 47 $ weye2,
			 translate 18 47 $ weye,translate 10 47 beye]
			 ++ hige1 3 ++ hige2 3)

hige1 :: Float -> [Picture]
hige1 0 = []
hige1 n = (rotate (40+15*n) $ line [(0,20),(0,60)]) : hige1 (n-1)

hige2 :: Float -> [Picture]
hige2 0 = []
hige2 n = (rotate ((-40)-(15*n)) $ line [(0,20),(0,60)]) : hige2 (n-1)