-- builder.hs

module Builder
  ( RNG, mkRNG
  , Individual(..)
  , GAConfig(..)
  , Population
  , FitnessFn
  , initPopulation
  , step
  , evolve
  ) where

import Data.List (sortOn)

-- minimal pure RNG (LCG)
newtype RNG = RNG { unRNG :: Int }

mkRNG :: Int -> RNG
mkRNG = RNG

nextInt :: RNG -> (Int, RNG)
nextInt (RNG s) =
  let a = 1664525
      c = 1013904223
      m = 2147483647
      s' = (a * s + c) `mod` m
  in (s', RNG s')

nextDouble :: RNG -> (Double, RNG)
nextDouble r =
  let (x, r') = nextInt r
      d = fromIntegral x / 2147483647.0
  in (d, r')

-- gaussian-ish step (sum of uniforms)
gaussStep :: RNG -> Double -> (Double, RNG)
gaussStep r std =
  let (a, r1) = nextDouble r
      (b, r2) = nextDouble r1
      (c, r3) = nextDouble r2
      z = (a + b + c - 1.5) * 2.0
  in (z * std, r3)

clamp :: Double -> Double -> Double -> Double
clamp lo hi x = max lo (min hi x)

type FitnessFn = [Double] -> Double

data Individual = Individual
  { genes   :: [Double]  
  , mutRate :: Double    
  , cxBlend :: Double    
  , lenBias :: Double   
  , fit     :: Double    
  } deriving (Show)

type Population = [Individual]

data GAConfig = GAConfig
  { popSize     :: Int
  , maxGen      :: Int
  , eliteCount  :: Int
  , addBaseProb :: Double
  , remBaseProb :: Double
  , opWeights   :: (Double, Double, Double)
  , initRange   :: (Double, Double)
  , stepStd     :: Double
  , minLen      :: Int
  , maxLen      :: Int
  }

-- weighted pick
pickWeightedIdx :: RNG -> [Double] -> (Int, RNG)
pickWeightedIdx r ws =
  let tot = sum ws
      cum = scanl1 (+) ws
      (u, r') = nextDouble r
      t = u * tot
      go i (c:cs) | t <= c    = i
                  | otherwise = go (i+1) cs
      go i [] = max 0 (i-1)
  in (go 0 cum, r')

initIndividual :: RNG -> GAConfig -> FitnessFn -> Int -> (Individual, RNG)
initIndividual r cfg f len0 =
  let n0 = max (minLen cfg) (min (maxLen cfg) len0)
      build 0 rr acc = (reverse acc, rr)
      build k rr acc =
        let (u, rr') = nextDouble rr
            (lo, hi) = initRange cfg
            g = lo + (hi - lo) * u
        in build (k-1) rr' (g:acc)
      (gs, r1) = build n0 r []
      (mrU, r2) = nextDouble r1
      (cxU, r3) = nextDouble r2
      (lbU, r4) = nextDouble r3
      mr  = 0.02 + 0.48 * mrU
      cx  = 0.10 + 0.80 * cxU
      lb  = lbU
      fi  = f gs
  in (Individual gs mr cx lb fi, r4)

initPopulation :: RNG -> GAConfig -> FitnessFn -> (Population, RNG)
initPopulation r cfg f =
  let baseLen = (minLen cfg + maxLen cfg) `div` 2
      go 0 rr acc = (reverse acc, rr)
      go k rr acc =
        let (ind, rr') = initIndividual rr cfg f baseLen
        in go (k-1) rr' (ind:acc)
  in go (popSize cfg) r []

-- tournament selection
tournament :: RNG -> Int -> Population -> (Individual, RNG)
tournament r k pop =
  let n = length pop
      pick rr 0 best = (best, rr)
      pick rr t best =
        let (u, rr') = nextDouble rr
            i = max 0 (min (n-1) (floor (u * fromIntegral n)))
            cand = pop !! i
        in pick rr' (t-1) (if fit cand > fit best then cand else best)
      (u0, r0) = nextDouble r
      i0 = max 0 (min (n-1) (floor (u0 * fromIntegral n)))
  in pick r0 (k-1) (pop !! i0)

-- mutation
mutate :: RNG -> GAConfig -> FitnessFn -> Individual -> (Individual, RNG)
mutate r cfg f ind =
  let s = stepStd cfg * (0.5 + mutRate ind)
      go rr [] acc = (reverse acc, rr)
      go rr (g:gs) acc =
        let (dv, rr') = gaussStep rr s
        in go rr' gs ((g + dv):acc)
      (gs', r1) = go r (genes ind) []
      (dM, r2) = gaussStep r1 0.05
      (dC, r3) = gaussStep r2 0.05
      (dL, r4) = gaussStep r3 0.10
      mr' = clamp 0.005 0.8 (mutRate ind + dM)
      cx' = clamp 0.05  0.95 (cxBlend ind + dC)
      lb' = clamp 0.0   1.0  (lenBias ind + dL)
      fi' = f gs'
  in (ind { genes = gs', mutRate = mr', cxBlend = cx', lenBias = lb', fit = fi' }, r4)

-- crossover
crossover :: RNG -> FitnessFn -> Individual -> Individual -> (Individual, RNG)
crossover r f a b =
  let alpha = clamp 0 1 ((cxBlend a + cxBlend b) / 2)
      ga = genes a
      gb = genes b
      maxL = max (length ga) (length gb)
      avgA = if null ga then 0 else sum ga / fromIntegral (length ga)
      pad xs = xs ++ replicate (maxL - length xs) avgA
      ga' = pad ga
      gb' = pad gb
      child = zipWith (\x y -> alpha * x + (1 - alpha) * y) ga' gb'
      (j1, r1) = nextDouble r
      (j2, r2) = nextDouble r1
      (j3, r3) = nextDouble r2
      mr' = clamp 0.005 0.8  ((mutRate a + mutRate b) / 2 + (j1 - 0.5) * 0.02)
      cx' = clamp 0.05  0.95 ((cxBlend a + cxBlend b) / 2 + (j2 - 0.5) * 0.02)
      lb' = clamp 0.0   1.0  ((lenBias a + lenBias b) / 2 + (j3 - 0.5) * 0.04)
      fi  = f child
  in (Individual child mr' cx' lb' fi, r3)

-- (add/remove genes)
constructive :: RNG -> GAConfig -> FitnessFn -> Individual -> (Individual, RNG)
constructive r cfg f ind =
  let n = length (genes ind)
      (uA, r1) = nextDouble r
      (uR, r2) = nextDouble r1
      addP = clamp 0 1 (addBaseProb cfg * (0.5 + lenBias ind))
      remP = clamp 0 1 (remBaseProb cfg * (0.5 + (1 - lenBias ind)))
      (lo, hi) = initRange cfg
      add rr xs =
        let (u, rr') = nextDouble rr
            g = lo + (hi - lo) * u
        in (xs ++ [g], rr')
      remove rr xs =
        if null xs then (xs, rr) else (take (n-1) xs, rr)
      (gs1, r3) =
        if uA < addP && n < maxLen cfg then add r2 (genes ind) else (genes ind, r2)
      (gs2, r4) =
        if uR < remP && length gs1 > minLen cfg then remove r3 gs1 else (gs1, r3)
      fi' = f gs2
  in (ind { genes = gs2, fit = fi' }, r4)

makeOffspring :: RNG -> GAConfig -> FitnessFn -> Population -> (Individual, RNG, (Int, Bool))
makeOffspring r cfg f pop =
  let (wM,wC,wK) = opWeights cfg
      (which, r0) = pickWeightedIdx r [wM,wC,wK]
      (p1, r1) = tournament r0 3 pop
      (p2, r2) = tournament r1 3 pop
      (child, r3) = case which of
        0 -> mutate r2 cfg f p1
        1 -> crossover r2 f p1 p2
        _ -> constructive r2 cfg f p1
      parentAvg = (fit p1 + fit p2) / 2
      success   = fit child >= parentAvg
  in (child, r3, (which, success))

adaptWeights :: GAConfig -> (Int,Int,Int) -> (Int,Int,Int) -> GAConfig
adaptWeights cfg (sM,sC,sK) (tM,tC,tK) =
  let (wM,wC,wK) = opWeights cfg
      upd w s t =
        let r = if t == 0 then 0 else fromIntegral s / fromIntegral t
        in clamp 0.05 10.0 (w * (0.8 + 0.4 * r))
      wM' = upd wM sM tM
      wC' = upd wC sC tC
      wK' = upd wK sK tK
      tot = wM' + wC' + wK'
      norm x = if tot <= 1e-12 then 1/3 else x / tot
  in cfg { opWeights = (norm wM', norm wC', norm wK') }

-- one generation
step :: RNG -> GAConfig -> FitnessFn -> Population -> (Population, RNG, GAConfig, (Int,Int,Int), (Int,Int,Int))
step r cfg f pop =
  let elites = take (eliteCount cfg) $ reverse $ sortOn fit pop
      need = popSize cfg - length elites
      go 0 rr acc sM sC sK tM tC tK = (reverse acc, rr, (sM,sC,sK), (tM,tC,tK))
      go k rr acc sM sC sK tM tC tK =
        let (child, rr', (opIdx, ok)) = makeOffspring rr cfg f pop
            (sM', sC', sK') = case (opIdx, ok) of
                                (0,True) -> (sM+1, sC,   sK)
                                (1,True) -> (sM,   sC+1, sK)
                                (2,True) -> (sM,   sC,   sK+1)
                                _        -> (sM,   sC,   sK)
            (tM', tC', tK') = case opIdx of
                                0 -> (tM+1, tC,   tK)
                                1 -> (tM,   tC+1, tK)
                                _ -> (tM,   tC,   tK+1)
        in go (k-1) rr' (child:acc) sM' sC' sK' tM' tC' tK'
      (offs, r1, s, t) = go need r [] 0 0 0 0 0 0
      cfg' = adaptWeights cfg s t
  in (elites ++ offs, r1, cfg', s, t)

evolve :: RNG -> GAConfig -> FitnessFn -> Population -> (Population, RNG, GAConfig, (Int,Int,Int), (Int,Int,Int))
evolve r cfg f pop0 =
  let go 0 rr cfg' p s t = (p, rr, cfg', s, t)
      go g rr cfg' p _ _ =
        let (p1, rr1, cfg1, s1, t1) = step rr cfg' f p
        in go (g-1) rr1 cfg1 p1 s1 t1
  in go (maxGen cfg) r cfg pop0 (0,0,0) (0,0,0)
