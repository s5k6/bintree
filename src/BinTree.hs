module BinTree
  ( BinTree(..), tfold, search, depth, serialize, isLeaf, mkBinTree, size
  ) where



data BinTree k v
  = Inner (BinTree k v) k (BinTree k v)
  | Leaf v
  deriving (Eq, Show)



tfold :: (a -> k -> a -> a) -> (v -> a) -> BinTree k v -> a

tfold inner leaf = go
  where
    go (Inner l k r) = inner (go l) k (go r)
    go (Leaf v) = leaf v



search :: Ord k => k -> BinTree k v -> v

search q = go
  where
    go n = case n of
      Leaf v -> v
      Inner l k r -> go (if q < k then l else r)



depth :: Integral a => BinTree k v -> a

depth = tfold (\l _ r -> 1 + max l r) (const 0)



size :: Integral a => BinTree k v -> a

size = tfold (\l _ r -> l + r) (const 1)



serialize :: BinTree k a -> (a, [(k, a)])

serialize = tfold f (\v -> (v, []))
  where
    f (l, ls) k (r, rs) = (l, ls ++ (k, r) : rs)



isLeaf :: BinTree k v -> Bool

isLeaf (Leaf _) = True
isLeaf _ = False



mkBinTree :: v -> [(k,v)] -> BinTree k v

mkBinTree v0 xs = deSerialize Inner (Leaf v0) [ (k, Leaf v) | (k, v) <- xs ]



deSerialize :: (t -> l -> t -> t) -> t -> [(l, t)] -> t

deSerialize mkNode = go True []
  where
    go !d acc !t0 input = case input of
      ((l1, !t1):(l2, !t2):rest) -> go d ((l2, tie d t0 l1 t1):acc) t2 rest
      [(l1, !t1)] -> go (not d) [] (tie d t0 l1 t1) acc
      [] -> if null acc then t0 else go (not d) [] t0 acc

    tie d t0 l1 t1 = if d then mkNode t0 l1 t1 else mkNode t1 l1 t0
