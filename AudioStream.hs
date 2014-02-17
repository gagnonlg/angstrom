{-# LANGUAGE FlexibleInstances #-}

module AudioStream 
(
      AudioStream, streamEval, discrete, writeStream8
    , WhiteNoise , whiteNoise
    , SinOsc, sinOsc
) 
where

import Data.Binary
import Data.Word()
import Data.IORef 
import Data.Time.Clock
import System.IO.Unsafe 

class AudioStream a where
    streamEval :: a -> Double -> Double

instance AudioStream (Double -> Double) where
    streamEval = id

combine :: (AudioStream a, AudioStream b) => 
    (Double -> Double -> Double) -> a -> b -> Double -> Double
combine op u v = \t -> streamEval u t `op` streamEval v t

infixr |+| 
(|+|) :: (AudioStream a, AudioStream b) => a -> b -> Double -> Double
(|+|) = combine (+)

infixr |*| 
(|*|) :: (AudioStream a, AudioStream b) => a -> b -> Double -> Double
(|*|) = combine (*)

discrete :: AudioStream a => Double -> Double -> a -> [Double]
discrete t0 dt g = streamEval g t0 : discrete (t0 + dt) dt g 

writeStream8 :: AudioStream a => FilePath -> Double -> Double -> a -> IO ()
writeStream8 p f t g = let st = take (floor (t*f)) $ discrete 0 (1/f) g
                       in encodeFile p $ map quantize8 st

constd :: Double -> Double -> Double
constd = const

data WhiteNoise = WhiteNoise {
    currentVal :: IORef Int
}

whiteNoise :: WhiteNoise
whiteNoise = WhiteNoise $ unsafePerformIO $ do
    time <- getCurrentTime
    let seed = floor $ utctDayTime time :: Int
    newIORef seed 

instance AudioStream WhiteNoise where
    streamEval g = const $ unsafePerformIO $ do 
        val <- readIORef $ currentVal g
        modifyIORef (currentVal g) lcg
        return $ scale (0,2^32) (-1,1) $ fromIntegral val

data SinOsc = SinOsc {
    freq_SinOsc  :: Double -> Double,
    phase_SinOsc :: Double -> Double 
}

sinOsc :: (AudioStream a, AudioStream b) => a -> b -> SinOsc
sinOsc f p = SinOsc (streamEval f) (streamEval p)

instance AudioStream SinOsc where
    streamEval s t = let w = 2*pi*(freq_SinOsc s t)
                         p = (phase_SinOsc s t)
                     in sin(w*t + p)


lcg :: Int -> Int
lcg = \x -> (a * x + c) `mod` m
    where a = 1664525
          c = 1013904223
          m = 2^32
    
scale :: Fractional a => (a,a) -> (a,a) -> a -> a
scale (x,y) (x',y') = let a = (y' - x') / (y - x)
                          b = x' - x*a
                      in \t -> a*t + b

quantize8 :: RealFrac a => a -> Word8
quantize8 = quantize 255

quantize :: (RealFrac a, Num b) => a -> a -> b 
quantize l = fromIntegral . floor. scale (-1,1) (0,l)
