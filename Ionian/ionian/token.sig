signature IONIAN_TOKENS =
sig
  type pos
  type token
  val ASSIGN : pos * pos -> token
  val IF : pos * pos -> token
  val THEN : pos * pos -> token
  val ELSE : pos * pos -> token
  val WHILE : pos * pos -> token
  val SKIP : pos * pos -> token
  val SEMICOLON : pos * pos -> token
  val ADD : pos * pos -> token
  val SUB : pos * pos -> token
  val MUL : pos * pos -> token
  val OR : pos * pos -> token
  val EQ : pos * pos -> token
  val LE : pos * pos -> token

  val INT : int * pos * pos -> token
  val ID : string * pos * pos -> token
  val EOF : pos * pos -> token
end
