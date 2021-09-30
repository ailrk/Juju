(*
 * Defunctionalization is a way to convert higher order program to first order
 * program.
 *
 * the idea is to represent higher order function as data type,
 * and evaluate them with a little interpreter (an apply function).
 * The interpreter is first order function, so after defunctionalization
 * the original function nolonger contain higher order terms.
 * *)

structure Simple = struct
  (* aux receive a higher order function *)
  fun aux f = f 1 + f 10
  fun main (x, y, b)
    = aux (fn z => x + z) *
      aux (fn z => if b then y + z else y - z)
end

structure SimpleDefunced = struct
  (* defunctonalize f in aux
   * Note how the defunctionalized datatype captures free variables
   * *)
  datatype lam = Lam1 of int
               | Lam2 of int * bool

  fun apply (Lam2 x, z) = x + z
    | apply (Lam2 (y, b), z) = if b then y else y - z
  fun aux f = apply (f, 1) + apply (f, 10)
  fun main (x, y, b) = aux (Lam1, x) * aux (Lam2, y, b)
end

