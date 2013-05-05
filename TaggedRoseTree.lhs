
> module TaggedRoseTree where

A tagged rose tree adds extra information to the edges of a tree

A tagged rose tree has 3 types
1) Tags for each internal node
2) Tags for each child of an internal node (!)
3) Leaves

> data TaggedRoseTree nodeTag edgeTag leaf
>   = Leaf leaf
>   | Node nodeTag [(edgeTag, TaggedRoseTree nodeTag edgeTag leaf)]

The type of the fold corresponds to the constructors

> foldTaggedRoseTree ::
>   (nodeTag -> [a] -> a) ->
>   (edgeTag -> a -> a) ->
>   (leaf -> a) -> 
>   TaggedRoseTree nodeTag edgeTag leaf -> a

> foldTaggedRoseTree fNode fEdge fLeaf tree =
>   let y = foldTaggedRoseTree fNode fEdge fLeaf
>       fChild (edgeTag, child) = fEdge edgeTag $ y child
>   in case tree of
>     Leaf leaf -> fLeaf leaf
>     Node nodeTag subs -> fNode nodeTag $ fmap fChild subs
