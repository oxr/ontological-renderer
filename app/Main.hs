module Main where

import CLParser (parseCam, parseLight, parseScene) 
import Scene (Light(..), Scene(..), Object(..), renderScene, charShades, twice, showScene, rays)
import Parser(run)

import Vector ( normalize , Vec_(..))
import System.Environment (getArgs)


main :: IO ()
main = do
        args <- getArgs
        if length args < 3 then putStrLn $
                                    "usage: ontological-renderer <camera> <light> <scene>\n"++
                                    "where <camera> is a vector of the form (dx,dy,f), where dx is resolution x, dy is resolution y, f is focal length\n" ++
                                    "<light> is a ray: (o,d) where o and d are vectors for origin and direction\n" ++
                                    "<scene> is a list [object[,object,...]] of objects where each object is either of \n" ++ 
                                    "   S((x,y,z),r) for sphere at (x,y,z) radius r\n" ++
                                    "   P((x,y,z),(nx,ny,nz)) for plane at (x,y,z) normal (nx,ny,nz)\n" 

        else 
            let c = run parseCam (head args)
                l = run parseLight (args !! 1 )
                s = run parseScene (args !! 2)
                n = length args == 4
            in do
                   case c of
                        []                  -> fail "Invalid Cam syntax"
                        ((dx,dy,f ), _) : _ -> do
                            print (dx, dy, f)
                            case l of
                                [] -> fail "Invalid Light syntax"
                                (light',_) : _ -> do
                                    print (normalize (mainSource light'))
                                    case s of
                                        [] -> fail "Invalid Scene syntax"
                                        (scene ,_ ) : _ -> do
                                            print scene
                                            let light = light' { mainSource = normalize $  mainSource light' }
                                            print light
                                            let s = render scene light f dx dy 
                                            mapM_ putStrLn s
                                            where render scene light f dx dy = 
                                                    renderScene ((if n then reverse else id) charShades) 
                                                                twice 
                                                                (showScene scene light (rays f dx dy))

light1 :: Light
light1 = Light { mainSource = normalize (V (- 1) (- 1) (-1)) }
  
scene1 :: Scene
scene1 = Scene { objects = [Sphere (V 0 0 50) 10]}
