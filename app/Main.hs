module Main where

import CLParser (parseCam, parseLight, parseScene) 
import Scene 
import Renderer
import Parser(run, int)

import Vector ( normalize , Vec_(..))
import System.Environment (getArgs)
import Codec.Picture
import GHC.Float (roundDouble)
import Data.List (find, genericLength)
import Data.Maybe (isJust)
import Data.Fixed (HasResolution(resolution))

help =  "usage: ontological-renderer <camera> <light> <scene> <pixel-size> <output-filename>\n"++
        "where <camera> is a vector of the form (dx,dy,f), where dx is resolution x, dy is resolution y, f is focal length\n" ++
        "<light> is a ray: (o,d) where o and d are vectors for origin and direction\n" ++
        "<scene> is a list [object[,object,...]] of objects where each object is either of \n" ++ 
        "   S((x,y,z),r) for sphere at (x,y,z) radius r\n" ++
        "   P((x,y,z),(nx,ny,nz)) for plane at (x,y,z) normal (nx,ny,nz)\n" ++
        "<pixel-size> is an integer\n" ++
        "<output-filename> is the filename the png is saved to\n"



main :: IO ()
main = do
        args <- getArgs
        if length args < 4 then putStrLn help 
        else 
            let c = run parseCam (head args)
                l = run parseLight (args !! 1 )
                s = run parseScene (args !! 2)
                ps = run int (args !! 3)
            in
            case c of
                [] ->  fail "Invalid Cam syntax"
                ((dx,dy,f ), _) : _ -> do
                    print (dx, dy, f)            
                    case l of 
                        [] -> fail "Invalid Light syntax" 
                        (light',_) : _ -> do
                            let light = light' { mainSource = normalize $  mainSource light' }
                            print light                            
                            case s of 
                                [] -> fail "Invalid Scene syntax"
                                (scene' ,_ ) : _ -> do 
                                    let pixelSize = case ps of 
                                                        [] -> 1
                                                        (n,_) : _ -> n
                                    let scene = scaleScene (1.0 / fromIntegral pixelSize) scene' 
                                    print scene'
                                    print scene
                                    print $ "Pixel size: " ++ show pixelSize
                                    main' dx dy f scene light pixelSize (args!!4)

                                  


main' :: Int -> Int -> Int -> Scene -> Light -> Int -> String -> IO ()
main' dx dy f scene light pixelSize filename = 
    writePng filename $ generateImage pixelRenderer (2*dx) (2*dy)
    where 
    pixelRenderer :: Int -> Int -> Pixel16 
    pixelRenderer x' y' = 
        let x = (x'`div` pixelSize) - dx `div` (pixelSize) 
            y = (y' `div` pixelSize) - dy `div` (pixelSize)
        in pixel16FromDouble (showRay scene light (Ray (V 0 0 0) (V (fromIntegral x) (fromIntegral y) (fromIntegral (f `div` pixelSize))))) 
    -- from <0,1> to <0,maxBound>
    pixel16FromDouble :: Double -> Pixel16  
    pixel16FromDouble d =  round (d *  fromIntegral topBound)
        where     
            topBound :: Pixel16
            topBound = maxBound


-- textual rendering
{-          let s = render scene light f dx dy 
            mapM_ putStrLn s
            where 
                render scene light f dx dy = 
                renderScene ((if n then reverse else id) charShades) twice (showScene scene light (rays f dx dy))
-}

            

transpose:: [[a]]->[[a]]
transpose ([]:_) = []
transpose x = map head x : transpose (map tail x)

avg :: Fractional a => [a] -> a -> a
avg [] a = a
avg l@(_:_) _ = sum l / genericLength l