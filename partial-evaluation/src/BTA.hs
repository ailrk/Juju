module Staging.BTA where

-- binding time analysis
-- http://www.cs.cmu.edu/~spot/nitrous/bta.html
-- https://reader.elsevier.com/reader/sd/pii/S0304397500000530?token=837E88A34687E8A8CD54BB481D60CFFD90F5BA2EABA9D7B1FCC06D8B015F99D67D7837C029415797CA2E2E91A93E5A83&originRegion=us-east-1&originCreation=20210916223218
-- Binding type analysis is a part of partial evaluation

-- BTA separates different parts of a program by their binding time.

-- e.g given a program, if the full information of it's argument can be known
-- statically, then we mark it with binding time S, on the other hand, if
-- some information parameter depends on the runtime behavior, we mark it as R.

-- Because functons call functions, the whole program will evenually form a
-- call graph. With tags added by BTA we can analyse which part of the program
-- can be executed at which stage.
-- PS: Stages can be generazlied to more than two! Like macro generating macros
-- S < D, so the call graph also forms a lattice.

data BT = S | D deriving (Show, Eq)



