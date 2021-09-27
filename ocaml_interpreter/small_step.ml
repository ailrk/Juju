(* a simple language with sos style reduction
   Asuming there is no free variables.

   # Call by value in structural operational style (SOS)
   Term a, b := N | x | \x.a | a b
   Values v ::= N | \x.a

   - one step reduction relation a -> a': -------------------------------------
      (\x.a) v -> a[x <- v]  (Betav)

        a -> a'                     b -> b'
     -------------(App-l)         -----------(App-r)
      a b -> a' b                  v b -> v b'

    - properties --------------------------------------------------------------
      1. it's a weak redution, we cannot reduce under lambda abstraction.
          e.g  no this rule
          a -> a'
        ------------X
        \x.a -> \x.a'

      2. call by value, for a redex (\x.a) b, b must be fully reduced to a value
         before beta reducition
         (\x.a) v -> a[x <- v]
         (\x.a)(b c) -> a[x <- b c] --X

      3. left to right application. In application a b, we need to reduce a
         to a value firsts before we can reduce b.
          b -> b'
        -------------
         v b -> v b'

      4.  deterministic. forall a, there is at most one a' such that a -> a'

    - Reduction sequences -----------------------------------------------------
      - Termination: a -> a1 -> ... -> v
      - Divergence: a -> a1 -> ... -> ...
      - Error: a -> a1 -> ... -/->

    - PS: also there is `Reduction context` which bascially doing the same thing

   # Call by name in SOS style
      (\x.a) b -> a[x <- b] (Betan)

        a -> a'
      ------------ (app-l)
      a b -> a' b

   - property -----------------------------------------------------------------
     1. does not evaluate argument before application
     2. beta reduction is performed as soon as possible.
     3. argument is evaluated when it's used.

   # CBV vs CBN
     1. M terminates in CBV -> M terminates in CBN
     2. some terms terminates in CBN doens't terminate in CBV
        e.g (\x.1) omega
              where omega = (\x.x x)(\x.x x)
        in CBV, omega will not terminate so (\x.1) reduction will never perform
        in CBN, substitution happens first, so we get 1 right away.
     3. We can encode CBN in CBV languages with thunks, but the reverse requires
        CPS transformation.

 *)

module SmallStepReduction = struct
  type term = Const of int
            | Var of string
            | Lam of string * term
            | App of term * term

  let isvalue = function
    | Const _ -> true
    | Lam _ -> true
    | _ -> false

  (* subst x with v in y
     Assume v has no free variable. Otherwise there will be name capturing.
   *)
  let rec subst x value = function
    | Const n -> Const n
    | Var y -> if x = y then value else Var y
    | Lam(arg, body) -> if x = arg then Lam(arg, body)
                                   else Lam(arg, subst x value body)
    | App(lam, arg) -> App(subst x value lam, subst x value arg)

  (* One step reduction in SOS style.
     apply only when a is a value, eval arguments first.
         (\x.a) v -> a[x <- v]  (Betav)

          a -> a'                     b -> b'
       -------------(App-l)         -----------(App-r)
        a b -> a' b                  v b -> v b'

      reduce perform reduction rule once at a time, return a partially recuded
      expression.
   *)
  let rec reduce = function
    | App(Lam(args, body), v) when isvalue v -> Some(subst args v body)
    | App(lam, arg) ->
        if isvalue lam then begin
          match reduce arg with
          | None -> None
          | Some arg' -> Some(App(lam, arg'))
        end else begin
          match reduce lam with
          | None -> None
          | Some lam' -> Some(App(lam', arg))
        end
    | _ -> None

  (* eval recursively reduce until nothing more to reduce *)
  let rec eval a =
    match reduce a with
      None -> a
    | Some a' -> eval a'
end
