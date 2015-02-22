module Transducer
    ( Transducer, TransducerProps
    , nothing, nothingProps
    , everything, everythingProps
    , fixingIndex, fixingIndexProps
    , mapping, mappingProps
    , keeping, keepingProps
    , dropping, droppingProps
    , keepingIf, keepingIfProps
    , droppingIf, droppingIfProps
    , keepingMod, keepingModProps
    , droppingMod, droppingModProps
    , keepingWhile, keepingWhileProps
    , droppingWhile, droppingWhileProps
    , splatting, splattingProps
    , compacting, compactingProps
    , flattening, flatteningProps
    , interposing, interposingProps
    , repeating, repeatingProps
    , dedupingById, dedupingByIdProps
    ) where


import Iter (Accumulator(..), Reducer, Iter, iterList, counter, done)


type alias Transducer a b r = (Reducer b r -> Reducer a r)
                            -- sink        -> reader
                            -- sink        -> i -> a -> acc

type alias TransducerProps =
  { indexRequired : Bool
  , indexPreserved : Bool
  , singleThread : Bool
  }


intMin = -2147483647 -- (2^31 * -1)


andThen : Transducer a b r -> Transducer b c r -> Transducer a c r
andThen ab bc =
  ab << bc


nothing : Transducer a a r
nothing sink i a acc =
  done acc

nothingProps =
  { indexRequired = False, indexPreserved = True, singleThread = False }


everything : Transducer a a r
everything sink i a acc =
  sink i a acc

everythingProps =
  { indexRequired = False, indexPreserved = True, singleThread = False }


fixingIndex : Transducer a a r
fixingIndex sink _ a acc =
  case acc of
    Counter i acc' -> counter (i + 1) (sink i a acc')
    Partial _ -> counter 1 (sink 0 a acc)
    Done _ -> acc

fixingIndexProps =
  { indexRequired = False, indexPreserved = True, singleThread = True }


mapping : (a -> b) -> Transducer a b r
mapping f sink i a acc =
  sink i (f a) acc

mappingProps =
  { indexRequired = False, indexPreserved = True, singleThread = False }


keepingIf : (a -> Bool) -> Transducer a a r
keepingIf pred sink _ a acc =
  if pred a
    then sink -1 a acc
    else acc

keepingIfProps =
  { indexRequired = False, indexPreserved = False, singleThread = False }


droppingIf : (a -> Bool) -> Transducer a a r
droppingIf pred =
  keepingIf (pred >> not)

droppingIfProps = keepingIfProps


keeping : Int -> Transducer a a r
keeping n sink i a acc =
  if | i < (n - 1) -> sink i a acc
     | i == (n - 1) -> done (sink i a acc)
     | otherwise -> done acc

keepingProps =
  { indexRequired = True, indexPreserved = True, singleThread = False }


reindexing : (Int -> Int) -> Transducer a a r
reindexing ij sink i a acc =
  let j = ij i
  in
      if j == intMin
        then acc
        else sink j a acc


dropping : Int -> Transducer a a r
dropping n =
  let ij i =
        if i >= n
          then i - n
          else intMin
  in
      reindexing ij

droppingProps =
  { indexRequired = True, indexPreserved = True, singleThread = False }


keepingMod : Int -> Transducer a a r
keepingMod n =
  let ij i =
        if i % n == 0
          then i // n
          else intMin
  in
      reindexing ij

keepingModProps =
  { indexRequired = True, indexPreserved = True, singleThread = False }


droppingMod : Int -> Transducer a a r
droppingMod n =
  let ij i =
        let m = i % n
        in
          if m == 0
            then intMin
            else (i // n) * (n - 1) + m - 1
  in
      reindexing ij

droppingModProps =
  { indexRequired = True, indexPreserved = True, singleThread = False }


keepingWhile : (a -> Bool) -> Transducer a a r
keepingWhile pred sink i a acc =
  if pred a
    then sink i a acc
    else done acc

keepingWhileProps =
  -- smart merge could make this not singleThread (at the cost of wasted work)
  { indexRequired = False, indexPreserved = True, singleThread = True }


droppingWhile : (a -> Bool) -> Transducer a a r
droppingWhile pred sink _ a acc =
  case acc of
    Counter 0 acc' -> if pred a
                        then acc
                        else counter 1 (sink 0 a acc')
    Counter i acc' -> counter (i + 1) (sink i a acc')
    Partial _ -> if pred a
                   then counter 0 acc
                   else counter 1 (sink 0 a acc)
    Done _ -> acc

droppingWhileProps =
  -- technically, restores index instead of preserves it
  { indexRequired = False, indexPreserved = True, singleThread = True }


compacting : Transducer (Maybe a) a r
compacting sink i maybe acc =
  case maybe of
    Just a -> sink -1 a acc
    _ -> acc

compactingProps =
  { indexRequired = False, indexPreserved = False, singleThread = False }


splatting' : Iter xs x r -> Transducer xs x r
splatting' iter sink i xs acc =
  iter xs sink acc

splatting : Iter xs x r -> Transducer xs x r
splatting iter =
  (splatting' iter) `andThen` (reindexing (always -1))

splattingProps =
  { indexRequired = False, indexPreserved = False, singleThread = False }


flattening : Transducer (List a) a r
flattening =
  splatting iterList

flatteningProps = splattingProps


interposing : a -> Transducer a a r
interposing delim sink i a acc =
  let acc' =
        if i <= 0
          then acc
          else sink (2 * i - 1) delim acc
  in
    case acc' of
      Done _ -> acc'
      _ -> sink (2 * i) a acc'

interposingProps =
  { indexRequired = True, indexPreserved = True, singleThread = False }


repeat : Int -> Transducer a a r
repeat n sink i a acc =
  if n == 0
    then acc
    else case acc of
           Done _ -> acc
           _ -> repeat (n - 1) sink (i + 1) a (sink i a acc) -- TC

repeating : Int -> Transducer a a r
repeating n sink i a acc =
  repeat n sink (n * i) a acc

repeatingProps =
  { indexRequired = False, indexPreserved = True, singleThread = False }


dedupingById : (a -> Int) -> Transducer a a r
dedupingById getId sink i a acc =
  let curr = getId a
  in
      case acc of
        Counter prev acc' -> if curr == prev
                               then acc
                               else counter curr (sink -1 a acc')
        Partial _ -> counter curr (sink -1 a acc)
        Done _ -> acc

dedupingByIdProps =
  -- very smart merge could make this not singleThread
  { indexRequired = False, indexPreserved = False, singleThread = True }
