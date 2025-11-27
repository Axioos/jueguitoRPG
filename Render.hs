{-# LANGUAGE RecordWildCards #-}

module Render
  ( render
  ) where


import Types


colorPlayer, colorWall, colorEnemy, colorItem, colorGoal, resetColor :: String
colorPlayer = "\ESC[33m"  
colorWall   = "\ESC[37m"  
colorEnemy  = "\ESC[31m"  
colorItem   = "\ESC[32m"  
colorGoal   = "\ESC[34m"  
resetColor  = "\ESC[0m"

cellChar :: GameState -> Int -> Int -> String
cellChar GameState{..} x y
  | (x,y) == playerPos = colorPlayer ++ "@" ++ resetColor
  | (x,y) `elem` walls = colorWall   ++ "#" ++ resetColor
  | (x,y) == goal      = colorGoal   ++ "T" ++ resetColor
  | (x,y) `elem` enemies = colorEnemy ++ "E" ++ resetColor
  | (x,y) `elem` items   = colorItem  ++ "*" ++ resetColor
  | otherwise            = "."

render :: GameState -> IO ()
render gs@GameState{..} = do
  putStr "\ESC[2J\ESC[H"
  putStrLn "=== Dungeon Haskell ==="
  putStrLn "WASD: mover | E: atacar | Q: salir\n"

  mapM_ putStrLn [ row y | y <- [0 .. height - 1] ]

  putStrLn ""
  putStrLn $ "Nivel: " ++ show level
  putStrLn $ "HP: " ++ show hp ++ "/" ++ show maxHp
           ++ "   Objetos: " ++ show collected ++ "/" ++ show totalItems
  putStrLn "Objetivo: reúne todos los * y llega al tesoro (T)."
  putStrLn $ "Mensaje: " ++ message
 where
  row y = concat [ cellChar gs x y | x <- [0 .. width - 1] ]
