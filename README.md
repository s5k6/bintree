
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

    mkBinTree mkNode = go True []
      where
        go d acc t0 input = case input of
          ((l1, t1):(l2, t2):rest) -> go d ((l2, tie d t0 l1 t1):acc) t2 rest
          [(l1, t1)] -> go (not d) [] (tie d t0 l1 t1) acc
          [] -> if null acc then t0 else go (not d) [] t0 acc
        tie d t0 l1 t1 = if d then mkNode t0 l1 t1 else mkNode t1 l1 t0

This function further abstracts from the type of binary trees.  It
just needs a function `mkNode :: t -> l -> t -> t` combining two trees
and a limit into a new tree.  And it assumes the values being
initially given as trees, as discussed above.

The first argument of `go` indicates the direction of list traversal.
Both directions are handled almost identically, except for the fact
that the node constructor `mkNode` must be fed its tree arguments
backwards when going right-to-left.  This is captured by the `tie`
function.

We use the accumulator pattern (`acc`, the second argument of `go`),
which nicely fits alternating the order in which the list is
traversed.


### Moving forward

As long as there are **at least two** two more pairs in the level, tie
the single tree `t0` with the tree `t1` from the *first* pair (also
consuming its limit `l1`), and put it on the accumulator together with
the limit `l2` from the *second* pair.  The traversal is easier to
understand if one imagines the accumulator backwards, and the entries
in its tuples flipped around.

The following show the first four steps of `go` while traversing right
through the input list (comprising the lowest (leaf) level of the
tree.

     t0   (l1, t1)  (l2, t2)  (l3, t3)  (l4, t4)  (l5, t5)  (l6, ...
       \__  __/
      (   l1    ,   l2)  t2   (l3, t3)  (l4, t4)  (l5, t5)  (l6, ...
           |               \__  __/
      (    |    ,   l2)   (   l3    ,   l4)  t4   (l5, t5)  (l6, ...
           |                   |               \__  __/
      (    |    ,   l2)   (    |    ,   l4)    (  l5    ,   l6)  ...

Everything to the left of the single tree is in the accumulator.  In
the code, the accumulator list is backwards, and its pair's components
are backwards too.


### At the end of the level

So at the end of the first left-to-right traversal, we are
(conceptually) in a situation like this one

    t0      t1      t2      t3           t_n-3   t_n-2         t_n-1   t_n
     \__  __/        \__  __/               \     /               \   /
    (   l1   , l2)  (   l3   , l4)  ...  (   l_n-2   , l_n-1)      l_n

if `n` was odd (i.e., odd number of limits, even number of trees).
This is the second case in the definition of `go`, i.e., the final
pair on the list (remeber: odd number of limits!) is tied with the
current single tree, forming the new single tree which now stands at
the very right.

The other case, that `n` was even, implies an odd number of trees, so
there's a leftover `t_n`.  That's the third case in the definition of
`go`, if the accumulator is not empty.  The current (last) single tree
has no pair to be tied with, so we leave it as it is:

    t0      t1      t2      t3           t_n-2  t_n-1
     \__  __/        \__  __/               \   /
    (   l1   , l2)  (   l3   , l4)  ...  (  l_n-1   , l_n)  t_n

In both cases, there's now a (backwards) list of trees with limits,
and a single tree at the right end.


### Turning around

Now, luckily, the pair components in the code are the other way round,
and hence match the input type of `go`.  So we could just feed the
remaining single node and the accumulator into `go`, and go again.
But wait: The node constructor needs its arguments flipped, lest the
order of the leaf nodes is flipped every second level.  So when
calling `go`, we negate the direction indicator `d`.

There's two places when we turn around: Either there are no pairs left
for this level (so just take the last tree and the accumulator), or
the level had an odd number of input pairs, so we tie the last one
with the single tree and use it as new single tree.  I.e., odd
nodes are tied first when turning around.  This actually balances the
tree =)


### Fin

The remaining case is when all pairs have been fused and only one tree
remains, which is when `go` retuns (in its third line).

Enjoy
