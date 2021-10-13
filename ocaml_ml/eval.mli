open Syntax

module type Eval = sig val eval : expr -> expr end
