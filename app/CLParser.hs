{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}

module CLParser where

import Parser
import Scene
import Vector



parseCam :: Parser (Int, Int, Int)
parseCam = triple int int int


parseSphere :: Parser Object
parseSphere = do 
    ((x,y,z),r, sface) <- triple (triple double double double) double surface
    return $ Object (Sphere (V x y z) r) sface

parsePlane :: Parser Object
parsePlane = do
                            ((x,y,z),(a,b,c), r) <- triple (triple double double double) (triple double double double) surface
                            return $ Object (Plane (Ray (V x y z) (V a b c))) r




surface :: Parser (Double, Cmyk)
surface = pair double cmyk


rgb :: Parser RGB
rgp = triple int int int

cmyk :: Parser Cmyk
cmyk = quadruple int int int int



parseObject :: Parser Object
parseObject = do 
                ch <- upper
                case ch of
                    'S' -> parseSphere
                    'P' -> parsePlane 
                    _ -> zero

                 

parseScene :: Parser Scene
parseScene = do { s <- list parseObject ; return $ Scene s }

parseLight :: Parser Light
parseLight = do { (x,y,z) <- triple double double double ; return $ Light (V x y z) }





