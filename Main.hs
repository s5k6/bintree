{-# LANGUAGE BangPatterns #-}

module Main where

import Control.DeepSeq ( NFData(..) )



data BinTree k v
  = Inner (BinTree k v) k (BinTree k v)
  | Leaf v
  deriving Show



tfold :: (a -> k -> a -> a) -> (v -> a) -> BinTree k v -> a

tfold inner leaf = go
  where
    go (Inner l k r) = inner (go l) k (go r)
    go (Leaf v) = leaf v



instance NFData (BinTree k v) where

  rnf = tfold (\(!l) (!k) (!r) -> ()) (\(!v) -> ())



search :: Ord k => k -> BinTree k v -> v

search q = go
  where
    go n = case n of
      Leaf v -> v
      Inner l k r -> go (if q < k then l else r)



depth :: BinTree k v -> Integer

depth = tfold (\l _ r -> 1 + max l r) (const 0)



serialize :: BinTree k a -> (a, [(k, a)])

serialize = tfold f (\v -> (v, []))
  where
    f (l, ls) k (r, rs) = (l, ls ++ (k, r) : rs)



isLeaf :: BinTree k v -> Bool

isLeaf (Leaf _) = True
isLeaf _ = False



addLeaves :: (v, [(l, v)]) -> (BinTree l v, [(l, BinTree l v)])

addLeaves (v0, xs) = (Leaf v0, [(k, Leaf v) | (k, v) <- xs])



mkInput :: Int -> (Int, [(Float, Int)])

mkInput n = (0, [(fromIntegral i, i) | i <- [1..n]])



testOrder :: Int -> Bool

testOrder n = (== i) . serialize . uncurry (mkBinTree Inner) $ addLeaves i
  where
    i = mkInput n



testDepth :: Integer -> Bool

testDepth n =
  (== n) . depth . uncurry (mkBinTree Inner) . addLeaves $ mkInput (2 ^ n - 1)



testFind :: Int -> Float -> Bool
testFind n k =
  (== floor k) . search k . uncurry (mkBinTree Inner) . addLeaves $ mkInput n



mkBinTree :: (t -> l -> t -> t) -> t -> [(l, t)] -> t

mkBinTree mkNode = go True []
  where
    go d acc t0 input = case input of
      ((l1, t1):(l2, t2):rest) -> go d ((l2, tie d t0 l1 t1):acc) t2 rest
      [(l1, t1)] -> go (not d) [] (tie d t0 l1 t1) acc
      [] -> if null acc then t0 else go (not d) [] t0 acc
    tie d t0 l1 t1 = if d then mkNode t0 l1 t1 else mkNode t1 l1 t0



main :: IO ()

main = putStrLn "hello"
