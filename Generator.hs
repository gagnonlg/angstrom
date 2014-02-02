module Generator where

import Data.Binary (encodeFile)
import Data.Word (Word8, Word16)

sinGen :: Floating a => (a -> a) -> (a -> a) -> a -> a
sinGen f g = \t -> let w = 2*pi*(f t)
                       p = g t
                   in sin(w*t + p)

discrete :: Num a => a -> a -> (a -> b) -> [b]
discrete t0 dt g = g t0 : discrete (t0 + dt) dt g 

quantize8 :: RealFrac a => a -> Word8
quantize8 = quantize 255

quantize16 :: RealFrac a => a -> Word16
quantize16 = quantize 65535

quantize :: (RealFrac a, Num b) => a -> a -> b 
quantize l = fromIntegral . floor. scale (-1,1) (0,l)

scale :: Fractional a => (a,a) -> (a,a) -> a -> a
scale (x,y) (x',y') = let a = (y' - x') / (y - x)
                          b = x' - x*a
                      in \t -> a*t + b

writeStream8 :: FilePath -> Double -> Double -> (Double -> Double) -> IO ()
writeStream8 p f t g = let st = take (floor (t*f)) $ discrete 0 (1/f) g
                           qst = map quantize8 st
                       in encodeFile p qst

