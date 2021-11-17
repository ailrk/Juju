module Test.P1 where


import Control.IndexedMonad

testIxStateT = runIxStateT c (0::Int) >>= print where
  c = vsget          `ibind` \v ->
      vsput (show v) `ibind` \_ ->
      vsget          `ibind` \v' ->
      ireturn (v, v')
