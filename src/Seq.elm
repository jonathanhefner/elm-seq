module Seq
    ( fromList, toList, toReverseList
    , fold, map
    , keep, drop
    , keepIf, dropIf
    , keepMod, dropMod
    , keepWhile, dropWhile
    , compact, flatten
    , interpose, repeat, dedupById
    ) where


import Native.Seq
import Iter (..)
import Transducer (..)
import List ((::))



type alias Seq' a r =
  { inducer : Inducer a r
  , indexPreserved : Bool
  }

type alias Seq a = Seq' a Existential

type Existential = Existential

elide : Seq' a r -> Seq' a Existential
elide = Native.Seq.elide

reify : r -> Seq' a Existential -> Seq' a r
reify = Native.Seq.reify



trans : Transducer a b r -> TransducerProps -> Seq' a r -> Seq' b r
trans t tProps s =
  if tProps.indexRequired && not s.indexPreserved
    then { inducer = s.inducer << fixingIndex << t
         , indexPreserved = tProps.indexPreserved }
    else { inducer = s.inducer << t
         , indexPreserved = s.indexPreserved && tProps.indexPreserved }


fromList : List a -> Seq a
fromList list =
  { inducer = iterList list
  , indexPreserved = True
  } |> elide


-- fromState : (st -> st) -> st -> st -> Seq st
-- fromRange : Int -> Int -> Seq Int
-- fromArray : Array a -> Seq a


fold : (a -> r -> r) -> r -> Seq a -> r
fold f r s =
  (reify r s).inducer (reducer f) (Partial r) |> unwrap


toReverseList : Seq a -> List a
toReverseList s =
  fold (::) [] s


toList : Seq a -> List a
toList s =
  Native.Seq.toList toReverseList s



map : (a -> b) -> Seq a -> Seq b
map f s =
  trans (mapping f) mappingProps s


keepIf : (a -> Bool) -> Seq a -> Seq a
keepIf pred s =
  trans (keepingIf pred) keepingIfProps s


dropIf : (a -> Bool) -> Seq a -> Seq a
dropIf pred s =
  trans (droppingIf pred) droppingIfProps s


keep : Int -> Seq a -> Seq a
keep n s =
  trans (keeping n) keepingProps s


drop : Int -> Seq a -> Seq a
drop n s =
  trans (dropping n) droppingProps s


keepMod : Int -> Seq a -> Seq a
keepMod n s =
  trans (keepingMod n) keepingModProps s


dropMod : Int -> Seq a -> Seq a
dropMod n s =
  trans (droppingMod n) droppingModProps s


keepWhile : (a -> Bool) -> Seq a -> Seq a
keepWhile pred s =
  trans (keepingWhile pred) keepingWhileProps s


dropWhile : (a -> Bool) -> Seq a -> Seq a
dropWhile pred s =
  trans (droppingWhile pred) droppingWhileProps s


compact : Seq (Maybe a) -> Seq a
compact s =
  trans compacting compactingProps s


-- splat : Iter xs x r -> Seq' xs r -> Seq' x r
-- splat iter s =
--  trans (splatting iter) splattingProps s


flatten : Seq (List a) -> Seq a
flatten s =
  trans flattening flatteningProps s


interpose : a -> Seq a -> Seq a
interpose delim s =
  trans (interposing delim) interposingProps s


repeat : Int -> Seq a -> Seq a
repeat n s =
  trans (repeating n) repeatingProps s


dedupById : (a -> Int) -> Seq a -> Seq a
dedupById getId s =
  trans (dedupingById getId) dedupingByIdProps s
