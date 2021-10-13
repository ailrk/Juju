open Syntax
exception Reduce
val evaluated : expr -> bool
val union : ('a -> 'b) -> ('a -> 'b) -> 'a -> 'b
val delta : expr -> expr
val beta : expr -> expr
val subst : var -> expr -> expr -> expr
val reduce : expr -> expr
