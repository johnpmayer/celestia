
> module Ship where
> import TaggedRoseTree

> data Beam = Beam Float

> data Part
>   = Engine Float
>   | FuelTank Float Float
>   | Processor Float

> data Attach = Attach Float Float

> type Ship = TaggedRoseTree Beam Attach Part
