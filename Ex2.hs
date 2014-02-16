module Main where

import AudioStream

main = white

white = do 
    putStrLn "white.raw..."
    writeStream8 "white.raw" 41000 10 whiteNoise

