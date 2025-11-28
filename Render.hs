{-# LANGUAGE RecordWildCards #-}

module Render (render) where

import Types

color :: String -> String -> String
color s c = "\ESC[" ++ c ++ "m" ++ s ++ "\ESC[0m"

cellChar :: GameState -> Int -> Int -> String
cellChar GameState{..} x y
  | (x,y) == playerPos   = color "@" "33"      
  | (x,y) `elem` walls   = color "#" "37"      
  | (x,y) `elem` enemies = color "E" "31"      
  | (x,y) == goal        = color "T" "34"     
  | (x,y) == portalA
    || (x,y) == portalB  = color "O" "36"      
  | otherwise =
      case lookup (x,y) items of
        Just QuestItem -> color "Q" "36"       
        Just Heart     -> color "H" "32"      
        Just Strength  -> color "S" "35"       
        Nothing        -> "."

render :: GameState -> IO ()
render gs@GameState{..} = do
  putStr "\ESC[2J\ESC[H"
  putStrLn "=== Dungeon RPG ==="
  putStrLn "WASD mover | E atacar | Q salir\n"

  mapM_ putStrLn
    [ concat [ cellChar gs x y | x <- [0 .. width-1] ]
    | y <- [0 .. height-1]
    ]

  putStrLn ""
  putStrLn $ "HP: "  ++ show hp ++ "/" ++ show maxHp
           ++ "  ATK: " ++ show atk
  putStrLn $ "Objetos de misión (Q): " ++ show questCollected
           ++ "/" ++ show totalQuest
  putStrLn ""
  putStrLn "Leyenda:"
  putStrLn "  @ = Jugador"
  putStrLn "  E = Enemigo"
  putStrLn "  Q = Objeto de misión (debes recoger todos)"
  putStrLn "  H = Cura 2 puntos de vida"
  putStrLn "  S = Aumenta tu fuerza (ATK, radio del ataque con E)"
  putStrLn "  T = Tesoro / salida (solo funciona si tienes todos los Q)"
  putStrLn "  O = Portal (teletransporta al otro portal)"
  putStrLn ""
  putStrLn $ "Mensaje: " ++ message
