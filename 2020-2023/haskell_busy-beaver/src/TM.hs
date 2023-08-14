{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE KindSignatures #-}
module TM where

-- turing machine

data  Tape state symbol = Tape state Int [symbol] deriving Eq
data Direction = L | R

newtype Transition state symbol = Transition
  { unTransition :: (state, symbol) -> (state, symbol, Direction) }

instance (Show state, Show symbol) => Show (Tape state symbol) where
  show (Tape s _ xs) = "state: " ++ show s ++ ", tape: " ++ show xs ++ "\n"

class TuringMachine (m :: * -> * -> *) where
  symbols      :: m state symbol -> [symbol]
  blankSym     :: m state symbol -> symbol
  inputSymbols :: m state symbol -> [symbol]
  states       :: m state symbol -> [state]
  initialState :: m state symbol -> state
  finalState   :: m state symbol -> state
  transition   :: m state symbol -> Transition state symbol

data TM state symbol = TM
  { tmSymbols      :: [symbol]
  , tmBlankSym     :: symbol
  , tmInputSymbols :: [symbol]
  , tmStates       :: [state]
  , tmInitialState :: state
  , tmFinalState   :: state
  , tmTransition   :: Transition state symbol
  }

instance TuringMachine TM where
  symbols = tmSymbols
  blankSym = tmBlankSym
  inputSymbols = tmInputSymbols
  states = tmStates
  initialState = tmInitialState
  finalState = tmFinalState
  transition = tmTransition

mkTape :: forall st sym . st -> Int -> sym -> Tape st sym
mkTape s n b = Tape s mid $ fmap (const b) [0..n]
  where mid = n `div` 2

tapeGoLeft :: forall st sym m. TuringMachine m => m st sym -> Tape st sym -> (Tape st sym, sym)
tapeGoLeft tm (Tape c n xs)
  | length xs == 0 = error "empty tape"
  | n == 0 = tapeGoLeft tm (Tape c n $ fmap (const (blankSym tm)) [1..32] ++ xs)
  | otherwise = (Tape c (n - 1) xs, xs !! (n - 1))

tapeGoRight :: forall st sym m . TuringMachine m => m st sym -> Tape st sym -> (Tape st sym, sym)
tapeGoRight tm (Tape c n xs)
  | length xs == 0 = error "empty tape"
  | n == length xs - 1 = tapeGoLeft tm (Tape c n $ xs ++ fmap (const (blankSym tm)) [1..32])
  | otherwise = (Tape c (n + 1) xs, xs !! (n + 1))

readCell :: forall st sym . Tape st sym -> sym
readCell (Tape _ n xs) = xs !! n

writeCell :: forall st sym . sym -> Tape st sym -> Tape st sym
writeCell s (Tape c n xs) = Tape c n (take n xs ++ [s] ++ drop (n + 1) xs)

setState :: forall st sym . st -> Tape st sym -> Tape st sym
setState newState (Tape _ n xs) = Tape newState n xs

stepTM :: forall st sym m
        . Eq st
       => TuringMachine m
       => m st sym
       -> Tape st sym
       -> Tape st sym
stepTM tm tape@(Tape currentState _ xs) =
  let tape1 = writeCell nextSym tape
      (tape2, _) = case dir of
                        L -> tapeGoLeft tm tape1
                        R -> tapeGoRight tm tape1
   in setState nextState tape2
  where
    trans = unTransition . transition $ tm
    (nextState, nextSym, dir) = trans (currentState, (readCell tape))

runTM :: forall st sym m . Eq st => TuringMachine m => m st sym -> [Tape st sym]
runTM tm = go [] tape
  where
    tape = mkTape (initialState tm) 32 (blankSym tm)
    shouldHalt (Tape c _ _) = c == finalState tm
    go acc tp
      | shouldHalt tp = acc ++ [tp]
      | otherwise = go (acc ++ [tp]) (stepTM tm tp)
