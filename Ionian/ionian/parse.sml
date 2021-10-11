(* just using the ml yacc *)

structure Parse : sig val parse : string -> Absyn.comb =
struct
  structure IonianLrVals IonianLrValsFun(structure Token = LrParser.Token)
  structure Lex IonianLexFun(structure Token = LrParser.Token)
  structure IonianP = Join(structure ParserData = IonianLrVals.Token,
                           structure Lex=Lex,
                           structure LrParser = LrParser)
  fun parse (filename: string) =
    let val _ = (Error.reset(); Error.fileName := filename)
        val file = TextIO.openIn filename
        fun get _ = TextIO.input file
        fun parseError (s, pos1, pos2) = Error.error pos1 s
        val lexer = LrParser.Stream.streamify (Lex.makeLexer get)
        val (absyn, _) = IonianP.parse(30, lexer, parseError, ())
    in TextIO.closeIn file;
       absyn
    end handle LrParser.ParseError => raise Error.Error
end
