-- process.hs
module Process
  ( fitness
  , target
  ) where

target :: [Double]
target = [0.3, -0.8, 1.2, 0.0, 0.9, -0.4, 0.5, -1.1]

fitness :: [Double] -> Double
fitness xs =
  let ys = target
      lx = length xs
      ly = length ys
      m  = max lx ly
      pad v n = v ++ replicate (n - length v) 0.0
      xs' = pad xs m
      ys' = pad ys m
      err = sum [ let d = xi - yi in d*d | (xi, yi) <- zip xs' ys' ]
      lenPenalty = fromIntegral (abs (lx - ly)) * 0.1
  in negate (err + lenPenalty)
