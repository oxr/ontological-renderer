module Main where

import CLParser (parseScene)
import Scene 
import Renderer
import Parser(run )
import qualified Config 
import Vector ( Vec_(..), (<+>), (<^*>), (<*^>), (</^>))
import System.Environment (getArgs)
import Codec.Picture
import Data.List (genericLength)
import Control.Monad.Except
import System.Random.Stateful
import qualified Data.Vector as V
import Data.Word (Word8)
import Codec.Picture.Types

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
    antiAliasing :: Int ,
    depth :: Int,
    skyColour :: Colour
}


main :: IO ()
main = do 
        args <- getArgs
        if length args < 1 then putStrLn help 
        else do
            x <- runExceptT $ do  
              cp <- Config.configFile (args!!0)
              ps <- Config.pixelSize cp
              cam <- Config.image cp
              light <- Config.light cp
              scale <- Config.scale cp
              scene <- Config.scene cp
              (r,g,b) <- Config.sky cp
              let skyy = V r g b
              aa <- Config.antiAliasing cp
              depth <- Config.depth cp
              amb <- Config.ambience cp
              return $ Config ps cam light {ambient = amb} scale scene aa depth skyy
            case x of 
                Left e -> fail (show e)
                Right cfg -> do
                    let c = camera cfg
                    let s = run parseScene (scenedef cfg)
                    case s of 
                        [] -> fail "Invalid Scene syntax"
                        (os ,_ ) : _ -> do 
                            let s = Scene os (skyColour cfg)
                            let scene = scaleScene (scale cfg / fromIntegral (pixelSize cfg)) s 
                            let filename = if length args < 2 then Nothing else Just (args!!1)
                            main' (resolution_x c) (resolution_y c) (focalLength c) scene (light cfg) (pixelSize cfg) (antiAliasing cfg) (depth cfg) filename
                            print "done."

-- for each (x,y) pixel, a vector of jitters
type JitterMap = V.Vector (V.Vector (V.Vector (Double, Double)))

generateJitterMap :: Int -> Int -> Int -> IO JitterMap
generateJitterMap dx dy spp = do
                                let jitters = rollDice spp
                                let line  = V.replicateM dx jitters
                                let matx =  V.replicateM dy line
                                matx
--    return ( V.replicateM dy (V.replicateM dx (rollDice spp)))
    where rollDice :: Int -> IO (V.Vector (Double,Double)) -- rollDice n-times 
          rollDice n = V.replicateM n roll1Dice
          roll1Dice :: IO (Double, Double)
          roll1Dice = do     
                        jx <- applyAtomicGen (uniformR (-0.5, 0.5)) globalStdGen  
                        jy <- applyAtomicGen (uniformR (-0.5, 0.5)) globalStdGen  
                        return (jx,jy)
          
main' :: Int -> Int -> Int -> Scene -> Light -> Int -> Int -> Int -> Maybe String -> IO ()
main' dx dy f scene light pixelSize jitters depth filename = do
    print $ "Generating jitter map: " ++ show (2*dx)  ++ " x " ++ show (2*dy)
    jitterMap <- generateJitterMap (2*dx) (2*dy) jitters
    print "Rendering..."
    case filename of 
        Nothing -> do
                let str = [ ( showCMYK8 . convertPixel $ pixelRenderer jitterMap x y )++ 
                            "(" ++ show x ++ "," ++ show y++")" ++ if x == 2*dx -1 then "\n" else "  "
                                | y <- [0..2*dy-1],x <- [0..2*dx-1] ]
                putStrLn $ concat str
        Just filename' -> writePng filename' $ generateImage (pixelRenderer jitterMap) (2*dx) (2*dy)
    where
    pixelRenderer :: JitterMap -> Int -> Int -> PixelRGB8
    pixelRenderer jitterMap x' y' = -- pixelation is simply div - going from a high resolution to a lower one by repetition of the same value 
        let x = (x' - dx) `div` pixelSize 
            y = (y' - dy) `div` pixelSize
            jitters = jitterMap V.! x' V.! y'
            rays = V.map (\(jx,jy) -> showRay scene light depth (Ray (V 0 0 0) (V (fromIntegral x + jx) (fromIntegral y + jy) (fromIntegral (f `div` pixelSize))))) jitters
            avgrays = foldr (<+>) (V 0 0 0) rays </^> fromIntegral (length rays)
        in 
            pixelFromDouble avgrays 
    showRGB8 :: PixelRGB8 -> String
    showRGB8 = show
    showCMYK8 :: PixelCMYK8 -> String
    showCMYK8 = show
    -- from <0,1> to <0,maxBound>
    pixelFromDouble :: Colour -> PixelRGB8
    pixelFromDouble (V r g b) = PixelRGB8 (round (fromIntegral maxword8 * r)) (round (fromIntegral maxword8 * g)) (round (fromIntegral maxword8 * b))
        where   maxword8 :: Word8
                maxword8 = maxBound 

transpose:: [[a]]->[[a]]
transpose ([]:_) = []
transpose x = map head x : transpose (map tail x)

avg :: Fractional a => [a] -> a -> a
avg [] a = a
avg l@(_:_) _ = sum l / genericLength l