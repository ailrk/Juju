(* ****************************************************************************
  SECD machine is a abstract machine for call-by-value semantics. It was first
  invented by Peter J Ladin in 1964 in his paper mechanical evaluation of
  expressions.

  Original SECD machine had four stacks: Stack, environment, code, dump. Modern
  SECD machine simplify the design and reduce the amount of stacks required
  to three. Namely we only need S: stack, E: environment, C: code. D: Dump was
  used for implementing function calls, but we can just use stack to do that
  alreay.
 *)


(* ****************************************************************************
   A naive SECD machine that just implements the base line semantics.
   environments for closure replace bound variables.
   We use deburjin index for representing variables. This way we don't need
   to worry about name capturing.
*)

module SECDMachineNaive = struct
  exception Error

  (* Note the debrujin index starts from 0 *)
  type inst =
    | LDV of value
    | ACCESS of int
    | CLOSURE of inst list
    | LET
    | ENDLET
    (* function calls *)
    | APPLY
    | RETURN

    (* basic arithmetics*)
    | ADD
    | SUB
  and value =
    | VInt of int
    | VClos of inst list * value list
    | VEnv of value list
    | VInst of inst list

    (* debugging case *)
    | VUnknown of value list * value list * inst list

  type environment = value list

  let interpreter code =
    let stk = [] in
    let (env: environment) = [] in
    let rec loop s e c = match s, e, c with
      | s, e, LDV n::cs -> loop (n::s) e cs
      | VInt(a)::VInt(b)::s, e, ADD::cs ->
          loop (VInt(a + b)::s) e cs

      | VInt(a)::VInt(b)::s, e, SUB::cs ->
          loop (VInt(a - b)::s) e cs

      | s, e, ACCESS(n)::cs -> loop (List.nth e n::s) e cs
      | (v::s), e, LET::cs -> loop s (v::e) cs
      | s, (_::e), ENDLET::cs -> loop s e cs
      | s, e, CLOSURE c'::cs -> loop (VClos(c', e)::s) e cs

      | (v::VClos(c', e')::ss), e, (APPLY::cs) ->
          loop (VInst(cs)::(VEnv e)::ss) (v::e') c'

      | (v::(VInst(c))::(VEnv e')::ss), _, RETURN::_ ->
          loop (v::ss) e' c

      | (v::_), _, [] -> v
      | s, e, c -> VUnknown (s, e, c)
    in loop stk env code
end

(* (\a. 2 + a) 1 *)
let t1 () =
  let open SECDMachineNaive in
  let p = [CLOSURE [ACCESS(0); LDV(VInt 2); ADD; RETURN]; LDV(VInt 1); APPLY]
  in interpreter p

(* ****************************************************************************
   The idea is simple:
   f = \. .. g 1 ..
   g = \. h(..)
   h = \. ..
   once g calls h, stack for g essentially useless, because when h returns
   there is nothing more for g to do.

   Avoiding extra return frame.
*)
module SECDTailCalled = struct
  exception Error

  (* Note the debrujin index starts from 0 *)
  type inst =
    | LDV of value
    | ACCESS of int
    | CLOSURE of inst list
    | LET
    | ENDLET

    (* function calls *)
    | APPLY
    | TAILAPPLY (* case for handling extra return frame *)
    | RETURN

    (* basic arithmetics*)
    | ADD
    | SUB
  and value =
    | VInt of int
    | VClos of inst list * value list
    | VEnv of value list
    | VInst of inst list

    (* debugging case *)
    | VUnknown of value list * value list * inst list

  type environment = value list

  let interpreter code =
    let stk = [] in
    let (env: environment) = [] in
    let rec loop s e c = match s, e, c with
      | s, e, LDV n::cs -> loop (n::s) e cs
      | VInt(a)::VInt(b)::s, e, ADD::cs ->
          loop (VInt(a + b)::s) e cs

      | VInt(a)::VInt(b)::s, e, SUB::cs ->
          loop (VInt(a - b)::s) e cs

      | s, e, ACCESS(n)::cs -> loop (List.nth e n::s) e cs
      | (v::s), e, LET::cs -> loop s (v::e) cs
      | s, (_::e), ENDLET::cs -> loop s e cs
      | s, e, CLOSURE c'::cs -> loop (VClos(c', e)::s) e cs

      | (v::VClos(c', e')::ss), _, (TAILAPPLY::_) -> loop ss (v::e') c'
      | (v::VClos(c', e')::ss), e, (APPLY::cs) ->
          loop (VInst(cs)::(VEnv e)::ss) (v::e') c'

      | (v::(VInst(c))::(VEnv e')::ss), _, RETURN::_ ->
          loop (v::ss) e' c

      | (v::_), _, [] -> v
      | s, e, c -> VUnknown (s, e, c)
    in loop stk env code
end

(* When compiling from frontend syntax, if we see a call is at tail position
   we transform it into a TAILAPPLY
   \x. (\y. y + 3) (x + 2) $ 1
 *)
let t2 () =
  let open SECDTailCalled in
  let p = [CLOSURE [
             CLOSURE [ACCESS(0); LDV(VInt 3); ADD; RETURN];
             ACCESS(0); LDV(VInt 2); ADD;
             TAILAPPLY; RETURN]; (* inner stack frame is discard *)
           LDV(VInt 1); APPLY]
  in interpreter p
