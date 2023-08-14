{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE LambdaCase #-}
module Beaver where

import TM

type State = Char
type Symbol = Char


-------------------------------------------------------------------------------
-- 3 state 2 symbol busy beaver
mkBB3 :: Transition State Symbol -> TM State Symbol
mkBB3 trans =
  TM { tmSymbols = ['_', '1']
     , tmBlankSym = '_'
     , tmInputSymbols = ['1']
     , tmStates = ['a', 'b', 'c', '!']
     , tmInitialState = 'a'
     , tmFinalState = '!'
     , tmTransition = trans
     }

transitionBB3Ver1 :: Transition State Symbol
transitionBB3Ver1 = Transition $ \case
  ('a', '_') -> ('b', '1', R)
  ('a', '1') -> ('c', '1', L)
  ('b', '_') -> ('a', '1', L)
  ('b', '1') -> ('b', '1', R)
  ('c', '_') -> ('b', '1', L)
  ('c', '1') -> ('!', '1', R)
  _ -> error "impossible state"

runBB3Ver1 = runTM (mkBB3 transitionBB3Ver1)
-- trace of 2 symbol 3 states busy beaver
-- [state: 'a', tape: "_________________________________"
-- ,state: 'b', tape: "________________1________________"
-- ,state: 'a', tape: "________________11_______________"
-- ,state: 'c', tape: "________________11_______________"
-- ,state: 'b', tape: "_______________111_______________"
-- ,state: 'a', tape: "______________1111_______________"
-- ,state: 'b', tape: "_____________11111_______________"
-- ,state: 'b', tape: "_____________11111_______________"
-- ,state: 'b', tape: "_____________11111_______________"
-- ,state: 'b', tape: "_____________11111_______________"
-- ,state: 'b', tape: "_____________11111_______________"
-- ,state: 'a', tape: "_____________111111______________"
-- ,state: 'c', tape: "_____________111111______________"
-- ,state: '!', tape: "_____________111111______________"]


-------------------------------------------------------------------------------
