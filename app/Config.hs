{-# LANGUAGE FlexibleContexts #-}
module Config (Config(..),config, configFile) where

import Data.ConfigFile
import Data.Either
import Control.Monad.Except
import Renderer (Cam(..))
import Vector (Vec_(..), normalize)
import Scene (Light(..), Colour)

type ConfigFileMonad = ExceptT CPError IO

-- type ConfigFileMonad = ExceptT CF.CPError IO

data Config = Config { 
    pixelSize :: Int , 
    camera :: Cam, 
    light :: Light,
    scale :: Double,
    scenedef :: String ,
    antiAliasing :: Int ,
    shake :: Bool, 
    depth :: Int,
    skyColour :: Colour ,
    vx :: Int,
    vy :: Int
}


configFile :: String -> ConfigFileMonad ConfigParser
configFile filename = do
                        cp <- join $ liftIO $ readfile emptyCP filename
                        return cp {accessfunc = interpolatingAccess 10}

config :: ConfigParser -> ConfigFileMonad Config
config cp = do
            ps <- get cp "Image" "pixelsize"
            cam <- getCam cp
            light <- getLight cp
            scale <- get cp "Scene" "scale"
            scene <- get cp  "Scene" "def"
            (r,g,b) <- get cp "Scene" "sky"
            let skyy = V r g b
            aa <- get cp "Image" "aa"
            sh <- get cp "Image" "shake"
            depth <- get cp "Image" "depth"
            amb <- get cp "Light" "ambience"
            (sx, sy) <- getViewAngle cp
            return $ Config ps cam light {ambient = amb} scale scene aa sh depth skyy sx sy

getCam :: MonadError CPError m => ConfigParser -> m Cam
getCam cp = do 
                resx <- get cp "Image" "resx"
                resy <- get cp "Image" "resy"
                f    <- get cp "Camera" "f"
                return $ Cam (V 0 0 0) (V 0 0 1) resx resy f 

getLight :: MonadError CPError m => ConfigParser -> m Light
getLight cp = do
            lx <- get cp "Light" "lightx"
            ly <- get cp "Light" "lighty"
            lz <- get cp "Light" "lightz"
            amb <- get cp "Light" "ambience"
            return $ Light (normalize (V lx ly lz)) amb

getViewAngle :: MonadError CPError m => ConfigParser -> m (Int, Int)
getViewAngle cp = do 
                sx <- get cp "Camera" "vx"
                sy <- get cp "Camera" "vy"
                return (sx,sy)
