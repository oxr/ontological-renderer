module Main where

import Parser (Parser, triple, int, run, list, pair, zero, upper, double)
import Scene 
import Vector ( Vec_(V), Vec , normalize)
import System.Environment (getArgs)

data Cam = Cam { dx :: Int , dy :: Int, f :: Int}


parseCam :: Parser (Int, Int, Int)
parseCam = triple int int int


parseSphere :: Parser Object
parseSphere = do 
    ((x,y,z),r) <- pair (triple double double double) double
    return $ Sphere (V x y z) r

parseObject :: Parser Object
parseObject = do 
                c <- upper
                case c of
                    'S' -> parseSphere
                    'P' -> do
                            ((x,y,z),(a,b,c)) <- pair (triple double double double) (triple double double double)
                            return $ Plane (Ray (V x y z) (V a b c))
                    _ -> zero

                 

parseScene :: Parser Scene
parseScene = do { s <- list parseObject ; return $ Scene s }

parseLight :: Parser Light
parseLight = do { (x,y,z) <- triple double double double ; return $ Light (V x y z) }


main :: IO ()
main = do
        args <- getArgs
        if (length args /= 3) then putStrLn $
                                    "usage: ontological-renderer <camera> <light> <scene>\n"++
                                    "where <camera> is a vector of the form (dx,dy,f), where dx is resolution x, dy is resolution y, f is focal length\n" ++
                                    "<light> is a ray: (o,d) where o and d are vectors for origin and direction\n" ++
                                    "<scene> is a list [object[,object,...]] of objects where each object is either of \n" ++ 
                                    "   S((x,y,z),r) for sphere at (x,y,z) radius r\n" ++
                                    "   P((x,y,z),(nx,ny,nz)) for plane at (x,y,z) normal (nx,ny,nz)\n" 

        else do 
            let ((dx,dy,f ), _) : _  = run  (parseCam) (args !! 0)
            print (dx,dy,f)
            let (light,_) : _ = run (parseLight) (args !! 1)
            print light
            let (scene ,_ ) : _ = run (parseScene) (args !! 2)
            print scene
            let s = renderScene charShades twice (showScene scene light (rays f dx dy))
            fmap (const ()) $ sequence (map putStrLn s)


light1 :: Light
light1 = Light { mainSource = normalize (V (- 1) (- 1) (-1)) }


  
scene1 :: Scene
scene1 = Scene { objects = [Sphere (V 0 0 50) 10]}
