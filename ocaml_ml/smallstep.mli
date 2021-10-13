open Syntax
module type SmallStepEval = sig val eval : expr -> expr end
module Stepped : SmallStepEval
module RecDecent : SmallStepEval
module EvalContext : SmallStepEval
