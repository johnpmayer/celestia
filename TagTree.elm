
module TagTree where

import open State

data TagTree leaf node edge 
  = Leaf leaf
  | Node node [(edge, TagTree leaf node edge)]

foldTagTree :
  (leaf -> a) ->
  (node -> [a] -> a) ->
  (edge -> a -> a) ->
  TagTree leaf node edge -> a
foldTagTree fLeaf fNode fEdge tree =
  let y = foldTagTree fLeaf fNode fEdge
      fChild (edge, child) = fEdge edge <| y child
  in case tree of
    Leaf leaf -> fLeaf leaf
    Node node subs -> fNode node <| map fChild subs

walkModify :
  (leaf -> State a leaf') ->
  (node -> State a node') ->
  (edge -> State a edge') ->
  TagTree leaf node edge ->
  State a (TagTree leaf' node' edge')
walkModify uLeaf uNode uEdge tree =
  let y = walkModify uLeaf uNode uEdge
      uSub (edge, sub) = 
        bindS (uEdge edge) (\edge' ->
        bindS (y sub) (\sub' ->
        returnS (edge', sub')))
  in case tree of
    Leaf leaf -> fmapS Leaf <| uLeaf leaf 
    Node node subs -> 
      bindS (mapMS uSub subs) (\subs' ->
      bindS (uNode node) (\node' ->
      returnS <| Node node' subs'))
