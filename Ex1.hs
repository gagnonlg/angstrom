module Main where

import Generator

main = writeStream8 "test.raw" 41000 10 $ sinGen (const 440) (const 0)
