
> module TaggedRoseTree where

A tagged rose tree adds extra information to the edges of a tree

A tagged rose tree has 3 types
1) Tags for each internal node
2) Tags for each child of an internal node (!)
3) Leaves

> data TaggedRoseTree leaf nodeTag edgeTag
>   = Leaf leaf
>   | Node nodeTag [(edgeTag, TaggedRoseTree leaf nodeTag edgeTag)]

The type of the fold corresponds to the constructors

> foldTaggedRoseTree ::
>   (leaf -> a) -> 
>   (nodeTag -> [a] -> a) ->
>   (edgeTag -> a -> a) ->
>   TaggedRoseTree leaf nodeTag edgeTag -> a

> foldTaggedRoseTree fLeaf fNode fEdge tree =
>   let y = foldTaggedRoseTree fLeaf fNode fEdge
>       fChild (edgeTag, child) = fEdge edgeTag $ y child
>   in case tree of
>     Leaf leaf -> fLeaf leaf
>     Node nodeTag subs -> fNode nodeTag $ fmap fChild subs
