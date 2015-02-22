module Iter
    ( Accumulator(..), Reducer, Inducer, Iter
    , iterList
    , reducer, counter, done, unwrap
    ) where


import Native.Iter


type Accumulator r = Counter Int (Accumulator r)
                   | Partial r
                   | Done r

type alias Reducer x r = (Int -> x -> Accumulator r -> Accumulator r)

type alias Inducer x r = Reducer x r -> Accumulator r -> Accumulator r
--         ^^^^^^^ in honor of Rich Hickey

type alias Iter xs x r = xs -> Inducer x r



iterList : Iter (List x) x r
iterList = Native.Iter.iterList


-- iterArray : Iter (Array x) x r
-- iterState : (st -> st) -> st -> Iter st st r
-- iterRange : Iter Int Int r


reducer : (a -> r -> r) -> Reducer a r
reducer f i a acc =
  case acc of
    Partial r -> Partial (f a r)
    _ -> acc -- shouldn't happen


counter : Int -> Accumulator r -> Accumulator r
counter i acc =
  case acc of
    Done _ -> acc
    _ -> Counter i acc


done : Accumulator r -> Accumulator r
done acc =
  case acc of
    Counter _ acc' -> done acc' -- TC
    Partial r -> Done r
    Done _ -> acc


unwrap : Accumulator r -> r
unwrap acc =
  case acc of
    Counter _ acc' -> unwrap acc' -- TC
    Partial r -> r
    Done r -> r
