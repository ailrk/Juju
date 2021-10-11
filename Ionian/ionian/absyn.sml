structure Absyn =
struct
datatype comb = Skip | Assign of Loc * aexpr | Seq of comb * comb
              | IfElse of bexpr * comb * comb | While of bexpr * comb
  and aexpr = Int of int | Var of loc | ABinary of Binop * aexpr * aexpr
  and bexpr = Bool of bool | BBinary of binop * bexpr * bexpr
    and loc = Loc of string
  and bexpr = Add | Sub | Mul | Le | And | Or | Eq

  fun add (x, y) = ABinary(Add, x, y)
  fun sub (x, y) = ABinary(Sub, x, y)
  fun mul (x, y) = ABinary(Mul, x, y)

  fun le (x, y) = BBinary(Le, x, y)
  fun and' (x, y) = BBinary(And, x, y)
  fun or' (x, y) = BBinary(Or, x, y)
end
