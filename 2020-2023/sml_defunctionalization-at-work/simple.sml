(*
 * Defunctionalization is a way to convert higher order program to first order
 * program.
 *
 * the idea is to represent higher order function as data type,
 * and evaluate them with a little interpreter (an apply function).
 * The interpreter is first order function, so after defunctionalization
 * the original function nolonger contain higher order terms.
 * *)


fun aux (i, f) = f i
fun main (i, js)
  = let fun walk nil = nil
          | walk (j::js) = (aux (i, fn i => i + j)) :: (walk js)
    in walk js
    end

fun run_main () = main 1 [1, 2, 3]


(* using data type to capture closure, and use an apply function to interpret
 * datatypes.
 * After this transformation there is nolonger higher order funtions.
 * *)
datatype lam = LAM of int
fun apply (LAM i, j) = i + j
fun aux_def (i, f) = apply (f, i)
fun main_def (i, js)
  = let fun walk nil = nil
          | walk (j :: js) = (aux_def (i, LAM j)) :: walk js
    in walk js
    end

