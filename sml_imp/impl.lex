type pos = int
type svalue = Tokens.svalue

type ('a, 'b) token = ('a, 'b) Tokens.token
type lexresult = (svalue, pos) token

val lineNum = Error.lineNum
val lineCol = Error.lineCol
fun err (p1, p2) = Error.error p1

fun eof () = let val pos = hd(!lineCol) in Tokens.EOF (pos, pos) end


%%
%header functor ImplLexFun(structure Tokens: IMPL_TOKEN);

%%
