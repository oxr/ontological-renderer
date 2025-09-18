module Main where

import CLParser (parseScene)
import Scene 
import Renderer
import Parser(run )
import qualified Config 
import Vector ( Vec_(..))
import System.Environment (getArgs)
import Codec.Picture
import Data.List (genericLength)
import Control.Monad.Except
import System.Random.Stateful

help :: [Char]
help =  "usage: ontological-renderer <config file> <outputfile.png>\n" ++
        "see or.conf for further help."

-- type ConfigFileMonad = ExceptT CF.CPError IO

data Config = Config { 
    pixelSize :: Int , 
    camera :: Cam, 
    light :: Light,
    scale :: Double,
    scenedef :: String ,
    antiAliasing :: Int }


main :: IO ()
main = do 
        args <- getArgs
        if length args < 2 then putStrLn help 
        else do
            x <- runExceptT $ do  
              cp <- Config.configFile (args!!0)
              ps <- Config.pixelSize cp
              cam <- Config.image cp
              light <- Config.light cp
              scale <- Config.scale cp
              scene <- Config.scene cp
              aa <- Config.antiAliasing cp
              return $ Config ps cam light scale scene aa
            case x of 
                Left e -> fail (show e)
                Right cfg -> do
                    let c = camera cfg
                    let s = run parseScene (scenedef cfg)
                    case s of 
                        [] -> fail "Invalid Scene syntax"
                        (scene' ,_ ) : _ -> do 
                            let scene = scaleScene (scale cfg / fromIntegral (pixelSize cfg)) scene' 
                            main' (resolution_x c) (resolution_y c) (focalLength c) scene (light cfg) (pixelSize cfg) (antiAliasing cfg) (args!!1)
                            print "done."

type JitterMap = Int -> Int -> [(Double, Double)]

generateJitterMap :: Int -> Int -> Int -> IO JitterMap
generateJitterMap dx dy spp = do -- samples per pixel
    array <- replicateM dy (replicateM dx (rollDice spp))
    return (\x y -> (array !! x) !! y)
    where roll1Dice :: IO (Double, Double)
          roll1Dice = do     
                        jx <- applyAtomicGen (uniformR (-0.5, 0.5)) globalStdGen  
                        jy <- applyAtomicGen (uniformR (-0.5, 0.5)) globalStdGen  
                        return (jx,jy)
          rollDice :: Int -> IO [(Double,Double)] -- rollDice n-times 
          rollDice n = replicateM n roll1Dice

        

main' :: Int -> Int -> Int -> Scene -> Light -> Int -> Int -> String -> IO ()
main' dx dy f scene light pixelSize jitters filename = do
    print $ "Generating jitter map: " ++ show (2*dx)  ++ " x " ++ show (2*dy)
    jitterMap <- generateJitterMap (2*dx) (2*dy) jitters
    print "Rendering..."
    writePng filename $ generateImage (pixelRenderer jitterMap) (2*dx) (2*dy)
    where
    pixelRenderer :: JitterMap -> Int -> Int -> Pixel16 
    pixelRenderer jitterMap x' y' = -- pixelation is simply div - going from a high resolution to a lower one by repetition of the same value 
        let x = (x' - dx) `div` pixelSize 
            y = (y' - dy) `div` pixelSize
            jitters = jitterMap x'  y'
            rays = map (\(jx,jy) -> showRay scene light (Ray (V 0 0 0) (V (fromIntegral x + jx) (fromIntegral y + jy) (fromIntegral (f `div` pixelSize))))) jitters
            avgrays = sum rays / fromIntegral (length rays)
        in 
            pixel16FromDouble avgrays 
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