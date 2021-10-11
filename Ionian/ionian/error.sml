signature ERROR =
sig
  exception Error

  val hasError : bool ref
  val fileName : string ref
  val lineNum : int ref
  val lineCol : int list ref
  val sourceStream : TextIO.instream ref
  val error : int -> string -> unit

  val impossible : string -> 'a
  val reset : unit -> unit
end


structure Error : ERROR =
struct
  val hasError = ref false
  val fileName = ref ""
  val lineNum = ref 1
  val lineCol = ref [1]
  val sourceStream = ref TextIO.stdIn

  exception Error

  fun reset () = (
    hasError := false;
    fileName := "";
    lineNum := 1;
    lineCol := [1];
    sourceStream := TextIO.stdIn)

  fun error pos (msg : string) =
    let fun look (x::xs, n) =
              if x < pos then
                app print [":", Int.toString n, ".", Int.toString (pos - x)]
              else look (xs, n - 1)
            | look _ = print "0.0"
    in hasError := true;
       print (!fileName);
       look (!lineCol, !lineNum);
       print (":" ^ msg ^ "\n")
    end

  fun impossible msg = (
    map print ["Error: ", msg, "\n"];
    TextIO.flushOut TextIO.stdOut;
    raise Error)
end
