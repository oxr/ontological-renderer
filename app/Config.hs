{-# LANGUAGE FlexibleContexts #-}
module Config where

import Data.ConfigFile
import Data.Either
import Control.Monad.Except
import Renderer (Cam(..))
import Vector (Vec_(..), normalize)
import Scene (Light(..))

type ConfigFileMonad = ExceptT CPError IO


configFile :: String -> ConfigFileMonad ConfigParser
configFile filename = join $ liftIO $ readfile emptyCP filename

pixelSize :: MonadError CPError m => ConfigParser -> m  Int
pixelSize cp = get cp "Image" "pixelsize"                

image :: MonadError CPError m => ConfigParser -> m Cam
image cp = do 
                resx <- get cp "Image" "resx"
                resy <- get cp "Image" "resy"
                f <- get cp "Image" "f"
                return $ 
                    Cam (V 0 0 0) (V 0 0 1) resx resy f 

light :: MonadError CPError m => ConfigParser -> m Light
light cp = do
            lx <- get cp "Light" "lightx"
            ly <- get cp "Light" "lighty"
            lz <- get cp "Light" "lightz"
            return $ Light $ normalize (V lx ly lz)

            
scale :: MonadError CPError m => ConfigParser -> m Double
scale cp = do 
            get cp "Scene" "scale"

scene :: MonadError CPError m => ConfigParser -> m String
scene cp = get cp "Scene" "def"

antiAliasing :: MonadError CPError m => ConfigParser -> m Int
antiAliasing cp = get cp "Image" "aa"

depth :: MonadError CPError m => ConfigParser -> m Int
depth cp = get cp "Image" "depth"