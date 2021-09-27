(* Execution model of programs
   1. interpretation:
      - control expressed in host langauge.
      - langauge represented as tree shaped data type.
      - the interpreter traverse the tree during execution
   2. compile to native code:
      - control is compiled to machine code before execution
      - stand alone binary
   3.  compile to abstract machine code.
      - control compiled to some abstract instructions
      - instructions are designed closer to operations of the
        source langauge.

   In haskell ppl usually write dsl, but what is dsl really?
   One way to think about it is to defunctionalize some functions
   to get a representation of a program, then we can write a
   eval function base on the tree like data type. It's like the
   reverse direction of lisp macro, to manipulate elements of
   the target langauge we hoist it's abstract syntax as somethingo
   first class and we can work with. (being data type means we
   have full control over it).

   Little interpreter is everywhere. A common technique like
   defunctionalize the continuaition and build up monad operation    is actually making a interpreter. e.g ReadP parser combinator.

   Another way to run program is to define some middle level
   hypothetical machine instructions. These instructions are
   designed to be close to the langauge so the translation is
   easy. As long as we fully implemented the abstarct machine
   our languge meet the spec completely.
 *)

(* a simple abstraction machine that executes arithmetic expressions.
   most ppl done this but it's good to see it in another pov.

    a := N | a1 + a2 | a1 - a2

    arch: one stack S

    inst:
      CONST(N) push N on stack
      ADD pop two ints push the sume
      SUB pop two ints push the difference.

    compliation:
      C(N) = CONST(N)
      C(a1 + a2) = C(a1):C(a2):ADD
      C(a1 - a2) = C(a1):C(a2):SUB
 *)

module SimpleArithmMachine = struct
  exception Error

  type inst = CONST | ADD | SUB | V of int
  type stack = inst list

  (* notion of the machine *)
  let push v s = v :: s

  let pop = function
    | (x::xs) -> (x, xs)
    | _ -> raise Error

  let interpreter code =
    let stk = [] in
    let rec loop stk code = match stk, code with
      | stk', (CONST::V n::code') -> loop (V n::stk') code'
      | (V n1::V n2::stk'), (ADD::code') -> loop (V (n1 + n2)::stk') code'
      | (V n1::V n2::stk'), (SUB::code') -> loop (V (n1 - n2)::stk') code'
      | [], [v] -> v
      | _ -> raise Error
    in loop stk code
end

