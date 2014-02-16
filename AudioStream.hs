{-# LANGUAGE FlexibleInstances #-}

import Data.IORef
import System.IO.Unsafe (unsafePerformIO)

class AudioStream a where
    
    streamEval :: a -> Double -> Double

    discrete :: Double -> Double -> a -> [Double]
    discrete t0 dt g = streamEval g t0 : discrete (t0 + dt) dt g 

type DGen = Double -> Double

instance AudioStream DGen where
    streamEval = id


data WhiteNoise = WhiteNoise_ {
    currentVal :: IORef Int
}

whiteNoise :: WhiteNoise
whiteNoise = WhiteNoise_ $ unsafePerformIO $ newIORef 1234


instance AudioStream WhiteNoise where
    streamEval g = const $ unsafePerformIO $ do 
        val <- readIORef $ currentVal g
        modifyIORef (currentVal g) lcg
        return $ scale (0,2^32) (-1,1) $ fromIntegral val

lcg :: Int -> Int
lcg = \x -> (a * x + c) `mod` m
    where a = 1664525
          c = 1013904223
          m = 2^32
    
scale :: Fractional a => (a,a) -> (a,a) -> a -> a
scale (x,y) (x',y') = let a = (y' - x') / (y - x)
                          b = x' - x*a
                      in \t -> a*t + b

sinGen :: AudioStream a => a -> a -> DGen
sinGen f g = \t -> let w = 2*pi*(streamEval f t)
                       p = streamEval g t
                   in sin(w*t + p)
