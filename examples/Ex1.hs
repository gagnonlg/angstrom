module Main where

import Generator

main = ex1 >> ex2 >> ex3

ex1 = writeStream8 "ex1.raw" 41000 10 $ sinGenC 440

ex2 = let phase = sinGenC 200
          gen   = sinGen (const 440) phase
      in writeStream8 "ex2.raw" 41000 10 gen

ex3 = let g1  = sinGenC 880 
          g2  = sinGenC 440
          gen = sequence1 5 g1 g2
      in writeStream8 "ex3.raw" 41000 10 $ gen
