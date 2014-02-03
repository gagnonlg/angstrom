module Generator where

import Data.Binary (encodeFile)
import Data.Word (Word8, Word16)

sinGen :: Floating a => (a -> a) -> (a -> a) -> a -> a
sinGen f g = \t -> let w = 2*pi*(f t)
                       p = g t
                   in sin(w*t + p)

sinGenC :: Floating a => a -> a -> a
sinGenC f = sinGen (const f) (const 0)

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
                       in encodeFile p $ map quantize8 st

combine :: (a -> b -> c) -> (t -> a) -> (t -> b) -> t -> c
combine op u v = \t -> u t `op` v t

infixr |+|
(|+|) :: Num b => (a -> b) -> (a -> b) -> a -> b
(|+|) = combine (+)

infixr |*|
(|*|) :: Num b => (a -> b) -> (a -> b) -> a -> b
(|*|) = combine (*)

sequence1 :: Ord t => t -> (t -> a) -> (t -> a) -> t -> a
sequence1 d1 g1 g2 = \t -> if t <= d1 then g1 t else g2 t
                    
after :: (Num a, Ord t) => t -> (t -> a) -> t -> a
after d = sequence1 d (const 0)

