-- | Contains commonly used functions
module Util.Common where

import Model.CommonTypes


formPath :: Hitbox -> [(Float, Float)]
formPath h = let pos = position h
                 box = head . boxes $ h
                 scale = scaling h
                 x = fst . unwrap $ pos
                 y = snd . unwrap $ pos
                 p1 = unwrap . fst . unwrapRectangle $ box
                 p2 = unwrap . snd . unwrapRectangle $ box
                 sx = fst . unwrap $ scale
                 sy = snd . unwrap $ scale
                 x1 = fst p1 * sx + x
                 y1 = snd p1 * sy + y
                 x2 = fst p2 * sx + x
                 y2 = snd p2 * sy + y
             in [(x1, y1), (x1, y2), (x2, y2), (x2, y1)]
