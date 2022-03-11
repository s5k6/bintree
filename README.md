
---
title: Balancing binary trees without comparing keys
---

> An ordered list contains all the information required to construct a
> balanced tree.  This method solely relies on the list structure to
> determine how to arrange the tree nodes.

You're a child, if under 20.  Then you're young.  Unless you're at
least 40, which makes you angry.  Unless you're 60 (or older), which
makes you wise.

An output range (of words) is partitioned by *intermediate* limiting
values.  Now quickly look up the word in a suitabe table.  Binary
trees, obvoiously:

    data BinTree k v
      = Inner (BinTree k v) k (BinTree k v)
      | Leaf v

Key?  Limit?  I use these terms exchangably in this context, it's kind
of a key for doing lookups, but it's much rather delimiting a whole
range of “real keys” mapping to the same value.

Thus, lookup:

    search :: Ord k => k -> BinTree k v -> v

    search q = go
      where
        go n = case n of
          Leaf v -> v
          Inner l k r -> go (if q < k then l else r)

How to build them trees?

Note, that the alternating nature of the input values *v* separated by
the limits *l*…

    v0, l1, v1, l2, v2, l3, v3, l4, v4

…can be captured by a type…

    input :: (v, [(l, v)])

…with `v` for the type of the values, which are separated by limits of
type `l`.  So the introductory example would be like

    ( "child", [(20, "young"), (40, "angry"), (60, "wise")]

Assuming that the list is ordered aptly, then it already **contains
all the knowledge required to construct the tree**.  Without actually
using the constraint `Ord k` at all, and without further comparing the
keys:

    v0, l1, v1, l2, v2, l3, v3, l4, v4, l5, v5, l6, v6, l7, v7
     \__  __/        \__  __/        \__  __/        \__  __/
        l1              l3              l5              l7
         \______  ______/                \______  ______/
                l2                              l6
                 \______________  ______________/
                                l4

See?  Just pair couples of consecutive values, taking the limit in
between to form an inner tree node.  **Skip a limit** and repeat.  At
the end of the list, start over, this time pairing trees instead of
values.  Repeat until the list has only one entry left.

It's actually easier if you see the values as tiny trees right from
the beginning, i.e., we have as input a list of trees separated by
limits, which is easy enough to construct:

    addLeaves :: (v, [(l, v)]) -> (BinTree l v, [(l, BinTree l v)])
    addLeaves (v0, xs) = (Leaf v0, [(l, Leaf v) | (l, v) <- xs])

This construction of trees also works for odd lists

    v0, l1, v1, l2, v2, l3, v3, l4, v4, l5, v5, l6, v6, l7, v7, l8, v8
     \__  __/        \__  __/        \__  __/        \__  __/       /
        l1              l3              l5              l7         /
         \______  ______/                \______  ______/         /
                l2                              l6               /
                 \______________  ______________/               /
                                l4                             /
                                 \________________________  __/
                                                          l8

Kinda skew, though.  We all know how important balance is, so we'd
rather have balanced trees.  Like that one:

    v0, l1, v1, l2, v2, l3, v3, l4, v4, l5, v5, l6, v6, l7, v7, l8, v8
     \__  __/        \__  __/        \__  __/        \__  __/       /
        l1              l3              l5              l7         /
         \               \______  ______/                \_____  _/
          \                     l4                             l8
           \____  ______________/                              /
                l2                                            /
                 \______________________________  ___________/
                                                l6

This is created just as before, with a slight twist: The alternating
levels are created by traversing the level's list beginning
alternatingly form the left and from the right end.

Now for the code:

    mkBinTree :: (t -> l -> t -> t) -> t -> [(l, t)] -> t

    mkBinTree tie = go []
      where
        go acc t0 ((l1, t1):(l2, t2):rest) = go ((l2, tie t0 l1 t1):acc) t2 rest
        go acc t0 [(l1, t1)] = mkBinTree tie' (tie t0 l1 t1) acc
        go [] t0 [] = t0
        go acc t0 [] = mkBinTree tie' t0 acc
        tie' l k r = tie r k l

This function further abstracts from the type of binary trees.  It
just needs a function `tie :: t -> l -> t -> t` combining two trees
and a limit into a new tree.  And it assumes the values being
initially given as trees, as discussed above.

We use the accumulator pattern (the `acc` argument of `go`), which
nicely fits alternating the order in which the list is traversed.

The first line of `go`'s definition: Starting at one end, `go acc`
sees a single tree `t0`, and a list of limits and trees `[(l1,t1)…`.
As long as there are **at least two** two more pairs in the level, tie
the single tree `t0` with the tree `t1` from the *first* pair (also
consuming its limit `l1`), and put it on the accumulator together with
the limit `l2` from the *second* pair.  The traversal is easier to
understand if one imagines the entries in the tuples in the
accumulator flipped around:

     go acc   t0 ((l1, t1):(l2,        t2) : rest)

        ((tie t0   l1  t1,  l2) : acc) t2    rest    -- pair twisted in code

Or, if I'd also flip around the accumulator list, resort to more
pseudo syntax, and just show the pairs and the singleton tree, as they
progress from left to right:

     t0   (l1, t1)  (l2, t2)  (l3, t3)  (l4, t4)  (l5, t5)  …
       \__  __/
      (   l1    ,   l2)  t2   (l3, t3)  (l4, t4)  (l5, t5)  …
           |               \__  __/
      (    |    ,   l2)   (   l3    ,   l4)  t4   (l5, t5)  …

Everything to the left of the singleton tree is in the accumulator.
In the code, the accumulator list is backwards, and its pair's
components are backwards too.

So at the end of the first left-to-right traversal, we are
(conceptually) in a situation like this one

     t0      t1        t2      t3           t_n-2   t_n-1
    ( \__  __/ , l2)  ( \__  __/ , l4)  …  (   \     /   , l_n)   t_n
         l1                l3                   l_n-1

with a singleton tree `t_n` at the end.

Now, luckily the pair components in the code are the other way round,
and hence match the input type of `go`.  So we could just feed the
remaining singleton node and the accumulator into `go`, and go again.
But wait: The node constructor needs its arguments flipped, lest the
order of the leaf nodes is flipped every second level.  So instead of
calling `go` when we turn around, we call `mkBinTree` with a twisted
node constructor `tie'`.

There's two places when we turn around: Either there are no pairs left
for this level (so just take the last tree and the accumulator), or
the level had an odd number of input pairs, so we tie the last one
with the singleton tree and use it as new singleton tree.  I.e., odd
nodes are tied first when turning around.  This actually balances the
tree =)

The remaining case is when all pairs have been fused and only one tree
remains, which is when `go` retuns (in its third line).

Enjoy
