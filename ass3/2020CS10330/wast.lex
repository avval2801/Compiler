structure T = Tokens

type pos = int
type svalue = T.svalue
type ('a, 'b) token = ('a, 'b) T.token
type lexresult = (svalue, pos) token
type lexarg = string
type arg = lexarg

val lin = ref 1;
val col = ref 0;
val eolpos = ref 0;

val badCh : string * string * int * int -> unit = fn
    (fileName,bad,line,col) =>
    TextIO.output(TextIO.stdOut,fileName^"["
        ^Int.toString line^"."^Int.toString col
        ^"] Invalid character \""^bad^"\"\n");
val eof = fn fileName => T.EOF (!lin,!col);

structure KeyWord :
sig val find : string ->
                (int * int -> (svalue,int) token) option
end =
struct
    val TableSize = 422 (* 211 *)
    val HashFactor = 5
    val hash = fn
        s => List.foldr (fn (c,v) =>
            (v*HashFactor+(ord c)) mod TableSize) 0 (explode s)
    val HashTable = Array.array(TableSize,nil) :
                    (string * (int * int -> (svalue,int) token))
                    list Array.array
    val add = fn
        (s,v) => let val i = hash s
                 in Array.update(HashTable,i,(s,v)
                    :: (Array.sub(HashTable, i)))
                 end
    val find = fn
        s => let val i = hash s
                 fun f ((key,v)::r) = if s=key then SOME v
                                      else f r
                    | f nil = NONE
             in f (Array.sub(HashTable, i))
             end
    val _ = (List.app add [
         ("program", T.PROGRAMM),
         ("var", T.VARR),
         ("int", T.INTT),
         ("bool", T.BOOLL),
         ("read", T.READD),
         ("write", T.WRITEE),
         ("if", T.IFF),
         ("then", T.THENN),
         ("else", T.ELSEE),
         ("endif", T.ENDIFF),
         ("while", T.WHILEE),
         ("do", T.DOO),
         ("endwh", T.ENDWHH),
         ("tt", T.TT),
         ("ff", T.FF)
        ])
end;

open KeyWord;


%%
%full
%header (functor WastLexFun(structure Tokens: Wast_TOKENS));
%arg (fileName:string);
%s WAST;
sym   = ("a" | "b" | "c" | "d" | "e" | "f" | "g" | "h" | "i" | "j" | "k" | "l" | "m" | "n" | "o" | "p" | "q" | "r" | "s" | "t" | "u" | "v" | "w" | "x" | "y" | "z" | "A" | "B" | "C" | "D" | "E" | "F" | "G" | "H" | "I" | "J" | "K" | "L" | "M" | "N" | "O" | "P" | "Q" | "R" | "S" | "T" | "U" | "V" | "W" | "X" | "Y" | "Z" )("0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" | "a" | "b" | "c" | "d" | "e" | "f" | "g" | "h" | "i" | "j" | "k" | "l" | "m" | "n" | "o" | "p" | "q" | "r" | "s" | "t" | "u" | "v" | "w" | "x" | "y" | "z" | "A" | "B" | "C" | "D" | "E" | "F" | "G" | "H" | "I" | "J" | "K" | "L" | "M" | "N" | "O" | "P" | "Q" | "R" | "S" | "T" | "U" | "V" | "W" | "X" | "Y" | "Z" )*;

numeral = ("0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9")+;
ws      = [\ \t];
eol     = ("\013\010"|"\010"|"\013");

%%
<INITIAL>{ws}* => (lin:=1; eolpos:=0;
                    YYBEGIN WAST; continue());
<WAST>{ws}* => (continue ());
<WAST>{eol} => (lin:=(!lin)+1;
                eolpos:=yypos+size yytext; continue ());
<WAST>{sym}+ => (case find yytext of
                    SOME v => (col:=yypos-(!eolpos);
                                v(!lin,!col))
                | _ => (col:=yypos-(!eolpos);
                        T.IDE(yytext,!lin,!col)));
<WAST>{numeral}+ => (col:=yypos-(!eolpos);
                        T.NUM(yytext,!lin,!col));

<WAST>"::" => (col:=yypos-(!eolpos); T.CONS(!lin,!col));
<WAST>":" => (col:=yypos-(!eolpos); T.COLON(!lin,!col));
<WAST>";" => (col:=yypos-(!eolpos); T.SCOLON(!lin,!col));
<WAST>"," => (col:=yypos-(!eolpos); T.COMMA(!lin,!col));
<WAST>"{" => (col:=yypos-(!eolpos); T.LBRACE(!lin,!col));
<WAST>"}" => (col:=yypos-(!eolpos); T.RBRACE(!lin,!col));
<WAST>":=" => (col:=yypos-(!eolpos); T.ASSIGN(!lin,!col));
<WAST>"~" => (col:=yypos-(!eolpos); T.NEG(!lin,!col));
<WAST>"||" => (col:=yypos-(!eolpos); T.ORR(!lin,!col));
<WAST>"&&" => (col:=yypos-(!eolpos); T.ANDD(!lin,!col));
<WAST>"(" => (col:=yypos-(!eolpos); T.LPAR(!lin,!col));
<WAST>")" => (col:=yypos-(!eolpos); T.RPAR(!lin,!col));
<WAST>"!" => (col:=yypos-(!eolpos); T.NOTT(!lin,!col));
<WAST>"<" => (col:=yypos-(!eolpos); T.LTT(!lin,!col));
<WAST>"<=" => (col:=yypos-(!eolpos); T.LEQQ(!lin,!col));
<WAST>"=" => (col:=yypos-(!eolpos); T.EQQ(!lin,!col));
<WAST>">" => (col:=yypos-(!eolpos); T.GTT(!lin,!col));
<WAST>">=" => (col:=yypos-(!eolpos); T.GEQQ(!lin,!col));
<WAST>"<>" => (col:=yypos-(!eolpos); T.NEQQ(!lin,!col));
<WAST>"+" => (col:=yypos-(!eolpos); T.PLUSS(!lin,!col));
<WAST>"-" => (col:=yypos-(!eolpos); T.MINUSS(!lin,!col));
<WAST>"*" => (col:=yypos-(!eolpos); T.MUL(!lin,!col));
<WAST>"/" => (col:=yypos-(!eolpos); T.DIVV(!lin,!col));
<WAST>"%" => (col:=yypos-(!eolpos); T.MODD(!lin,!col));
<WAST>. => (col:=yypos-(!eolpos);
            badCh (fileName,yytext,!lin,!col);
            T.BADCH(!lin,!col));