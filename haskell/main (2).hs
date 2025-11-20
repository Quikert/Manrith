-- main.hs

module Main where

import Builder
import Process (fitness)
import System.IO (isEOF, hGetContents, stdin)
import System.Process (readProcess)
import Data.List (sortOn)
import Text.Read (readMaybe)

readTokens :: IO [(String, String)]
readTokens = do
  contents <- hGetContents stdin
  let ls = lines contents
      parseLine s =
        case break (== '=') s of
          (k, '=':v) -> Just (k, v)
          _          -> Nothing
  return [ kv | Just kv <- map parseLine ls ]

findNum :: Read a => String -> a -> [(String, String)] -> a
findNum key def toks =
  case lookup key toks of
    Just v -> maybe def id (readMaybe v)
    Nothing -> def

best :: Population -> Individual
best = foldl1 (\a b -> if fit a >= fit b then a else b)

showInd :: Individual -> String
showInd ind =
  let gs = genes ind
      preview = take 10 gs
      more = if length gs > 10 then "..." else ""
      avg = if null gs then 0 else sum gs / fromIntegral (length gs)
  in "len=" ++ show (length gs)
     ++ " fit=" ++ show (fit ind)
     ++ " mut=" ++ show (mutRate ind)
     ++ " cx="  ++ show (cxBlend ind)
     ++ " lenBias=" ++ show (lenBias ind)
     ++ " avgGene=" ++ show avg
     ++ " genes=" ++ show preview ++ more

phase :: Int -> RNG -> GAConfig -> (Population, RNG, GAConfig)
phase gens r cfg =
  -- run cfg.maxGen
  let f = fitness
      
      in error "phase: should not be called without initial population"

main :: IO ()
main = do
  toks <- readTokens
  let seed    = findNum "SEED" (12345 :: Int) toks
      wM      = findNum "WM"   (0.34 :: Double) toks
      wC      = findNum "WC"   (0.33 :: Double) toks
      wK      = findNum "WK"   (0.33 :: Double) toks
      minLen  = findNum "MINLEN" (2 :: Int) toks
      maxLen  = findNum "MAXLEN" (24 :: Int) toks
      stepStd = findNum "STEPSTD" (0.15 :: Double) toks
      lo      = findNum "RANGE_LO" (-1.0 :: Double) toks
      hi      = findNum "RANGE_HI" (1.0 :: Double) toks
      addP    = findNum "ADD_BASE" (0.25 :: Double) toks
      remP    = findNum "REM_BASE" (0.20 :: Double) toks

  let cfg0 = GAConfig
        { popSize     = 60
        , maxGen      = 200
        , eliteCount  = 4
        , addBaseProb = addP
        , remBaseProb = remP
        , opWeights   = (wM, wC, wK)
        , initRange   = (lo, hi)
        , stepStd     = stepStd
        , minLen      = minLen
        , maxLen      = maxLen
        }
      rng0 = mkRNG seed
      f    = fitness

  -- initialize population
  let (p0, r0) = initPopulation rng0 cfg0 f

  -- phase A
  let (pA, rA, cA, sA, tA) = evolve r0 cfg0 f p0
  putStrLn $ "After " ++ show (maxGen cfg0) ++ " generations:"
  putStrLn $ showInd (best pA)

  -- ask C autobuilder to adapt weights based on success counts
  let cmdLineA = "SM=" ++ show (let (x,_,_) = sA in x)
              ++ " SC=" ++ show (let (_,x,_) = sA in x)
              ++ " SK=" ++ show (let (_,_,x) = sA in x)
              ++ " TM=" ++ show (let (x,_,_) = tA in x)
              ++ " TC=" ++ show (let (_,x,_) = tA in x)
              ++ " TK=" ++ show (let (_,_,x) = tA in x)
  wOutA <- readProcess "../c/autobuilder" [] cmdLineA
  let kvsA = [ (k, v) | l <- lines wOutA
                      , let (k, rest) = break (== '=') l
                      , not (null rest)
                      , let v = tail rest ]
      wM1 = maybe wM id (readMaybe =<< lookup "WM" kvsA)
      wC1 = maybe wC id (readMaybe =<< lookup "WC" kvsA)
      wK1 = maybe wK id (readMaybe =<< lookup "WK" kvsA)
      c1  = cA { opWeights = (wM1, wC1, wK1) }

  -- phase B
  let (pB, rB, cB, sB, tB) = evolve rA c1 f pA
  putStrLn $ "After " ++ show (maxGen c1) ++ " generations (phase 2):"
  putStrLn $ showInd (best pB)

  -- phase C (another adaptation)
  let cmdLineB = "SM=" ++ show (let (x,_,_) = sB in x)
              ++ " SC=" ++ show (let (_,x,_) = sB in x)
              ++ " SK=" ++ show (let (_,_,x) = sB in x)
              ++ " TM=" ++ show (let (x,_,_) = tB in x)
              ++ " TC=" ++ show (let (_,x,_) = tB in x)
              ++ " TK=" ++ show (let (_,_,x) = tB in x)
  wOutB <- readProcess "../c/autobuilder" [] cmdLineB
  let kvsB = [ (k, v) | l <- lines wOutB
                      , let (k, rest) = break (== '=') l
                      , not (null rest)
                      , let v = tail rest ]
      wM2 = maybe wM1 id (readMaybe =<< lookup "WM" kvsB)
      wC2 = maybe wC1 id (readMaybe =<< lookup "WC" kvsB)
      wK2 = maybe wK1 id (readMaybe =<< lookup "WK" kvsB)
      c2  = cB { opWeights = (wM2, wC2, wK2) }

  let (pC, _rC, cC, _sC, _tC) = evolve rB c2 f pB
  putStrLn $ "+ After " ++ show (maxGen c2) ++ " generations (phase 3):"
  putStrLn $ showInd (best pC)
  putStrLn $ "+ Final operator weights: " ++ show (opWeights cC)
