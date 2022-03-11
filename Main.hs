module Main where



data BinTree k v
  = Inner (BinTree k v) k (BinTree k v)
  | Leaf v
  deriving Show


tfold :: (a -> k -> a -> a) -> (v -> a) -> BinTree k v -> a
tfold inner leaf = go
  where
    go (Inner l k r) = inner (go l) k (go r)
    go (Leaf v) = leaf v

depth = tfold (\l _ r -> 1 + max l r) (const 0)

linear = tfold (\l k r -> l ++ k : r) pure

flat = tfold f (\v -> (v, []))
  where
    f (l, ls) k (r, rs) = (l, ls ++ (k, r) : rs)


addLeaves :: (v, [(l, v)]) -> (BinTree l v, [(l, BinTree l v)])

addLeaves (v0, xs) = (Leaf v0, [(k, Leaf v) | (k, v) <- xs])



mkInput :: Int -> (Int, [(Float, Int)])
mkInput n = (0, [(fromIntegral i, i) | i <- [1..n]])

testOrder n = (== i) . flat . uncurry (mkBinTree Inner) $ addLeaves i
  where
    i = mkInput n

testDepth n =
  (== n) . depth . uncurry (mkBinTree Inner) . addLeaves $ mkInput (2 ^ n - 1)

testFind n k =
  (== floor k) . search k . uncurry (mkBinTree Inner) . addLeaves $ mkInput n

search :: Ord k => k -> BinTree k v -> v

search q = go
  where
    go n = case n of
      Leaf v -> v
      Inner l k r -> go (if q < k then l else r)



mkBinTree :: (t -> l -> t -> t) -> t -> [(l, t)] -> t

mkBinTree tie = go []
  where
    go acc t0 ((l1, t1):(l2, t2):rest) = go ((l2, tie t0 l1 t1):acc) t2 rest
    go acc t0 [(l1, t1)] = mkBinTree tie' (tie t0 l1 t1) acc
    go [] t0 [] = t0
    go acc t0 [] = mkBinTree tie' t0 acc
    tie' l k r = tie r k l


main :: IO ()
main = putStrLn "hello"
