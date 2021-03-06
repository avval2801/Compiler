structure Wast :
sig val compile : string -> DataTypes.Ast
end = 
struct
exception WastError;
fun compile (fileName) =
    let val inStream = TextIO.openIn fileName;
        val grab : int -> string = fn
            n => if TextIO.endOfStream inStream
                 then ""
                 else TextIO.inputN (inStream, n);
        val printError : string * int * int -> unit = fn
            (msg, line, col) =>
            print (fileName^"["^Int.toString line^":"^Int.toString col^"] "^msg^"\n");
        val (tree, rem) = WastParser.parse
                    (15,
                    (WastParser.makeLexer grab fileName),
                    printError,
                    fileName)
            handle WastParser.ParseError => raise WastError;

        val _ = TextIO.closeIn inStream;
    in tree
    end
end;

