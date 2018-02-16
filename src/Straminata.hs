module Straminata where

import Graphics.Gloss

data Game = Game

run :: IO ()
run = play
      (InWindow "straminata project" (800, 600) (10, 10))
      white
      30
      Game
      (const picture)
      (\_ _ -> Game)
      (\_ _ -> Game)

picture :: Picture
picture = Translate (-170) 0 $ Scale 0.5 0.5 $ Text "Hello World"
