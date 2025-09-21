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
    ((x,y,z),r, refl) <- triple (triple double double double) double double
    return $ Object refl (Sphere (V x y z) r)

parsePlane :: Parser Object
parsePlane = do
                            ((x,y,z),(a,b,c), r) <- triple (triple double double double) (triple double double double) double
                            return $ Object r (Plane (Ray (V x y z) (V a b c)))


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





