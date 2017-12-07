import Graphics.Gloss
import Data.Monoid

main :: IO ()
main = display (InWindow "Picture test" (400,400) (20,20))
       white
       (frac_p 5)


frac_p :: Integer -> Picture 
frac_p n = go 1 1 doraemon
       where
	 size = 100
         go sc l pic | l >= n = pic
	 go sc l pic = 
	     let newsl = 2*sc + 1
	     	 scinv  = 1/newsl
	      	 psmall = scale scinv scinv doraemon
		 plarge = scale (1-scinv) (1-scinv) pic
	     	 presult = pictures ((translate (size*(0-scinv)) (size*(0-scinv)) plarge) : [translate (size*(scinv-1)+i*2*size*scinv) ((1-scinv)*size) psmall | i <- [0..newsl-1]] ++ [translate (size*(1-scinv)) ((scinv-1)*size+i*2*size*scinv) psmall | i <- [0..newsl-1]] )
	     in go newsl (l+1) presult




 
doraemon :: Picture
doraemon =
	  let weye = circle 18
	      beye = color black $ circleSolid 10
	      nouse = color red $ circleSolid 10
	      mouse = circleSolid 40
	      zinchu = line [(0,-6),(0,20)]
	      dammy1 = color white $ rectangleSolid 200 33
	      dammy2 = color white $ rectangleSolid 100 50
	      bl = circleSolid 95
	      wh = circleSolid 70 in
	      pictures ([color blue $ bl,color white $ wh,
	      	         translate 0 (-78) dammy1,
			 color red $ mouse,translate 0 20 $ dammy2,
			 color black $ zinchu, translate 0 20 nouse,
			 translate (-18) 45 $ weye,translate (-10) 45 $ beye,
			 translate 18 45 $ weye,translate 10 45 beye]
			 ++ hige1 3 ++ hige2 3)

hige1 :: Float -> [Picture]
hige1 0 = []
hige1 n = (rotate (40+15*n) $ line [(0,20),(0,60)]) : hige1 (n-1)

hige2 :: Float -> [Picture]
hige2 0 = []
hige2 n = (rotate ((-40)-(15*n)) $ line [(0,20),(0,60)]) : hige2 (n-1)
