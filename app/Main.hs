module Main where

import CLParser (parseScene)
import Scene
import Renderer
import Parser(run )
import Vector ( Vec_(..), (<+>), (</^>))
import System.Environment (getArgs)
import Codec.Picture
import Data.List (genericLength)
import Control.Monad.Except
import System.Random.Stateful
import qualified Data.Vector as V
import Data.Word (Word8)
import Codec.Picture.Types
import Config (Config(..), config, configFile)
import Data.Char (readLitChar)

help :: [Char]
help =  "usage: ontological-renderer <config file> <outputfile.png>\n" ++
        "see or.conf for further help."



main :: IO ()
main = do
        args <- getArgs
        if null args then putStrLn help
        else do
            x <- runExceptT $ do
              cp <- configFile (args!!0)
              config cp
            case x of
                Left e -> fail (show e)
                Right cfg -> do
                    case run parseScene (scenedef cfg) of
                        [] -> fail "Invalid Scene syntax"
                        (os ,_ ) : _ -> do
                            let s' = Scene os (skyColour cfg)
                            let scene = scaleScene (scale cfg / fromIntegral (pixelSize cfg)) s'
                            let filename = if length args < 2 then Nothing else Just (args!!1)
                            main' cfg scene filename
                            -- controlLoop cfg scene filename
--                            (resolution_x c) (resolution_y c) (focalLength c) scene (light cfg) (pixelSize cfg) (abs $ antiAliasing cfg) (antiAliasing cfg < 0) (depth cfg) filename
    where 
        controlLoop :: Config -> Scene -> Maybe String -> IO ()
        controlLoop cfg scene filename = do 
            main' cfg scene filename
            c <- getChar
            case c of 
                'f' -> do 
                    let cam = camera cfg
                    controlLoop (cfg { camera = cam { focalLength = focalLength cam + 10} } ) scene filename
                'q' -> do 
                    putStrLn "done."
                _ -> controlLoop cfg scene filename


--main' :: Int -> Int -> Int -> Scene -> Light -> Int -> Int -> Bool -> Int -> Maybe String -> IO ()
main' :: Config -> Scene -> Maybe String -> IO ()
main' cfg scene filename =
    let dx = resolution_x (camera cfg)
        dy = resolution_y (camera cfg)
        jitters = antiAliasing cfg
    in do
        print $ "Generating jitter map: " ++ show (2*dx)  ++ " x " ++ show (2*dy)
        jitterMap <- generateJitterMap (2*dx) (2*dy) jitters --(2*dx `div` pixelSize) (2*dy `div` pixelSize) jitters
        print "Rendering..."
        case filename of
            Nothing         -> putStrLn $ writeCSV (pixelRenderer cfg scene jitterMap ) (showCMYK8 . convertPixel) (2*dx) (2*dy)
            Just filename'  -> writePng filename' $ generateImage (pixelRenderer cfg scene jitterMap) (2*dx) (2*dy)



-- for each (x,y) pixel, a vector of jitters
type JitterMap = V.Vector (V.Vector (V.Vector (Double, Double)))

generateJitterMap :: Int -> Int -> Int -> IO JitterMap
generateJitterMap dx dy spp = do
                                let jitters = rollDice spp
                                let line  = V.replicateM dy jitters
                                let matx =  V.replicateM dx line
                                matx
--    return ( V.replicateM dy (V.replicateM dx (rollDice spp)))
    where rollDice :: Int -> IO (V.Vector (Double,Double)) -- rollDice n-times 
          rollDice n = V.replicateM n roll1Dice
          roll1Dice :: IO (Double, Double)
          roll1Dice = do
                        jx <- applyAtomicGen (uniformR (-0.5, 0.5)) globalStdGen
                        jy <- applyAtomicGen (uniformR (-0.5, 0.5)) globalStdGen
                        return (jx,jy)

pixelRenderer :: Config -> Scene -> JitterMap -> Int -> Int -> PixelRGB8
pixelRenderer cfg scene jitterMap x y = -- pixelation is simply div - going from a high resolution to a lower one by repetition of the same value 
        let rx = resolution_x (camera cfg)
            ry = resolution_y (camera cfg)
            dx' = rx `div` pixelSize cfg
            dy' = ry `div` pixelSize cfg
            x' =  x  `div` pixelSize cfg
            y' =  y  `div` pixelSize cfg
            stepx = fromIntegral (vx cfg) / fromIntegral dx'
            stepy = fromIntegral (vy cfg) / fromIntegral dy'
            f = focalLength (camera cfg)
            jitters = jitterMap V.! (if shake cfg then x else x') V.! (if shake cfg then y else y') -- using x , y instead of x', y' makes the pixels jump 
            rays' = V.map (\(jx,jy) -> showRay scene (light cfg) (depth cfg) (Ray (V 0 0 0) (V ((fromIntegral (x' - dx') + jx ) * stepx)  ((fromIntegral (y' - dy') + jy) * stepy) (fromIntegral (f `div` pixelSize cfg))))) jitters
            avgrays = foldr (<+>) (V 0 0 0) rays' </^> fromIntegral (length rays')
        in
            pixelFromDouble avgrays

pixelFromDouble :: Colour -> PixelRGB8
pixelFromDouble (V r g b) = PixelRGB8 (round (fromIntegral maxword8 * r)) (round (fromIntegral maxword8 * g)) (round (fromIntegral maxword8 * b))
    where   maxword8 :: Word8
            maxword8 = maxBound


writeText :: (Int -> Int -> PixelRGB8) -> Int -> Int -> String
writeText pR dx dy =
            let str = [ ( showCMYK8 . convertPixel $ pR x y )++
                        "(" ++ show x ++ "," ++ show y++")" ++ if x == 2*dx -1 then "\n" else "  "
                        | y <- [0..2*dy-1],x <- [0..2*dx-1] ]
            in concat str

writeCSV :: (Pixel a ) => (Int -> Int -> a) -> (a -> String) -> Int -> Int -> String
writeCSV pR sh dx dy =
    let xheader = "," ++ (concat [ show i ++ "," | i <- [0..dx-1]])++ "\n"
        ls = [ show y ++ "," ++ (concat  [( "\"" ++ sh (pR x y)) ++ "\"," | x <- [0..dx-1] ]) ++ "\n" | y <- [0..dy-1]]
    in xheader ++ concat ls



showRGB8 :: PixelRGB8 -> String
showRGB8 = show

showCMYK8 :: PixelCMYK8 -> String
showCMYK8 (PixelCMYK8 c' m' y' k')= let
                                        c = c' `div` 16
                                        m = m' `div` 16
                                        y = y' `div` 16
                                        k = k' `div` 16
                                    in
    "(" ++ show c ++ "," ++ show m ++ "," ++ show y ++ "," ++ show k ++ ")"


transpose:: [[a]]->[[a]]
transpose ([]:_) = []
transpose xs = map head xs : transpose (map tail xs)

avg :: Fractional a => [a] -> a -> a
avg [] a = a
avg l@(_:_) _ = sum l / genericLength l