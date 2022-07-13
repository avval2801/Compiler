open DataTypes

(*function to check equality between strings*)
fun equalString(string1, string2) =
if string1 = string2 then true else false;

(*exceptions to be raised*)
exception VariableNotFound;
exception TypeIncorrect;

(*HashTable initiated so that type checking can be done*)
val hashTab : (string, int) HashTable.hash_table = HashTable.mkTable (HashString.hashString, equalString) (101, VariableNotFound);

(*Functions to add variables along with their types - 0 for int and 1 for bool to the hash table*)
fun addDec(varlist, typ) =
if(varlist = []) then DEC(varlist, typ)
else if(typ = "int") then
let val x = HashTable.insert hashTab (hd(varlist), 0); val y = addDec(tl(varlist), typ)
in
DEC(varlist, typ)
end
else if(typ = "bool") then
let val y = HashTable.insert hashTab (hd(varlist), 1); val y = addDec(tl(varlist), typ)
in
DEC(varlist, typ)
end
else raise TypeIncorrect;

(*Type of a variable is determined by the type specified in the hash table*)
fun typeVar(variable) = HashTable.lookup hashTab variable;

(*Type of expression is determined by its structures and the types of its sub expressions, hence recursion is used to infer types*)
fun typeExp(NEGATIVE(exp)) = if(typeExp(exp)=0) then 0 else raise TypeIncorrect
    | typeExp(PLUS(exp1, exp2)) = if(typeExp(exp1)=0 andalso typeExp(exp2)=0) then 0 else raise TypeIncorrect
    | typeExp(MINUS(exp1, exp2)) = if(typeExp(exp1)=0 andalso typeExp(exp2)=0) then 0 else raise TypeIncorrect
    | typeExp(TIMES(exp1, exp2)) = if(typeExp(exp1)=0 andalso typeExp(exp2)=0) then 0 else raise TypeIncorrect
    | typeExp(DIV(exp1, exp2)) = if(typeExp(exp1)=0 andalso typeExp(exp2)=0) then 0 else raise TypeIncorrect
    | typeExp(MOD(exp1, exp2)) = if(typeExp(exp1)=0 andalso typeExp(exp2)=0) then 0 else raise TypeIncorrect
    | typeExp(LOGICALNOT(exp)) = if(typeExp(exp)=1) then 1 else raise TypeIncorrect
    | typeExp(OR(exp1, exp2)) = if(typeExp(exp1)=1 andalso typeExp(exp2)=1) then 1 else raise TypeIncorrect
    | typeExp(AND(exp1, exp2)) = if(typeExp(exp1)=1 andalso typeExp(exp2)=1) then 1 else raise TypeIncorrect
    | typeExp(LT(exp1, exp2)) = if(typeExp(exp1) = typeExp(exp2)) then 1 else raise TypeIncorrect
    | typeExp(LEQ(exp1, exp2)) = if(typeExp(exp1) = typeExp(exp2)) then 1 else raise TypeIncorrect    
    | typeExp(EQ(exp1, exp2)) = if(typeExp(exp1) = typeExp(exp2)) then 1 else raise TypeIncorrect
    | typeExp(GT(exp1, exp2)) = if(typeExp(exp1) = typeExp(exp2)) then 1 else raise TypeIncorrect
    | typeExp(GEQ(exp1, exp2)) = if(typeExp(exp1) = typeExp(exp2)) then 1 else raise TypeIncorrect
    | typeExp(NEQ(exp1, exp2)) = if(typeExp(exp1) = typeExp(exp2)) then 1 else raise TypeIncorrect
    | typeExp(VAR(variable)) = typeVar(variable)
    | typeExp(NUMBER(numeral)) = 0
    | typeExp(TT) = 1
    | typeExp(FF) = 1

(*Functions that will return the value that will be an attribute in the attribute grammar rules below after doing type checking*)
fun funSET(variable, exp) = if(typeVar(variable) = typeExp(exp)) then SET(variable, exp) else raise TypeIncorrect;
fun funWRITE(exp) = if(typeExp(exp) = 0) then WRITE(exp) else raise TypeIncorrect;
fun funITE(exp, cmds1, cmds2) = if(typeExp(exp)=1) then ITE(exp, cmds1, cmds2) else raise TypeIncorrect;
fun funWH(exp, cmds) = if(typeExp(exp)=1) then WH(exp, cmds) else raise TypeIncorrect;
fun funNEGATIVE(exp) = if(typeExp(exp)=0) then NEGATIVE(exp) else raise TypeIncorrect;
fun funPLUS(exp1, exp2) = if(typeExp(exp1) = 0 andalso typeExp(exp2) = 0) then PLUS(exp1, exp2) else raise TypeIncorrect;
fun funMINUS(exp1, exp2) = if(typeExp(exp1) = 0 andalso typeExp(exp2) = 0) then MINUS(exp1, exp2) else raise TypeIncorrect;
fun funTIMES(exp1, exp2) = if(typeExp(exp1) = 0 andalso typeExp(exp2) = 0) then TIMES(exp1, exp2) else raise TypeIncorrect;
fun funDIV(exp1, exp2) = if(typeExp(exp1) = 0 andalso typeExp(exp2) = 0) then DIV(exp1, exp2) else raise TypeIncorrect;
fun funMOD(exp1, exp2) = if(typeExp(exp1) = 0 andalso typeExp(exp2) = 0) then MOD(exp1, exp2) else raise TypeIncorrect;
fun funLOGICALNOT(exp) = if(typeExp(exp) = 1) then LOGICALNOT(exp) else raise TypeIncorrect;
fun funOR(exp1, exp2) = if(typeExp(exp1) = 1 andalso typeExp(exp2) = 1) then OR(exp1, exp2) else raise TypeIncorrect;
fun funAND(exp1, exp2) = if(typeExp(exp1) = 1 andalso typeExp(exp2) = 1) then AND(exp1, exp2) else raise TypeIncorrect;
fun funLT(exp1, exp2) = if(typeExp(exp1) = typeExp(exp2)) then LT(exp1, exp2) else raise TypeIncorrect;
fun funLEQ(exp1, exp2) = if(typeExp(exp1) = typeExp(exp2)) then LEQ(exp1, exp2) else raise TypeIncorrect;
fun funGT(exp1, exp2) = if(typeExp(exp1) = typeExp(exp2)) then GT(exp1, exp2) else raise TypeIncorrect;
fun funGEQ(exp1, exp2) = if(typeExp(exp1) = typeExp(exp2)) then GEQ(exp1, exp2) else raise TypeIncorrect;
fun funEQ(exp1, exp2) = if(typeExp(exp1) = typeExp(exp2)) then EQ(exp1, exp2) else raise TypeIncorrect;
fun funNEQ(exp1, exp2) = if(typeExp(exp1) = typeExp(exp2)) then NEQ(exp1, exp2) else raise TypeIncorrect;
%%
%name Wast

%term IDE of string | CONS | LBRACE | RBRACE | COLON | SCOLON | COMMA | ASSIGN | NEG | PLUSS | MINUSS | MUL | DIVV | MODD
    | LPAR | RPAR | NOTT | ORR | ANDD | LTT | LEQQ | EQQ | GTT | GEQQ | NEQQ | NUM of string | PROGRAMM | VARR | INTT
    | BOOLL | READD | WRITEE | IFF | THENN | ELSEE | ENDIFF | WHILEE | DOO | ENDWHH | TT | FF | BADCH | EOF

%nonterm begin of Ast |  block of Block | declrlist of Declr list | declr of Declr | typ of Typ | varlist of Variable list
        | cmds of CmdSeq | cmdd of Cmd | exp of Exp | variable of Variable | numeral of Numeral

%pos int
%eop EOF
%noshift EOF
%nonassoc SCOLON COMMA LBRACE RBRACE LPAR RPAR 
%left LTT LEQQ NEQQ GEQQ GTT EQQ
%left PLUSS MINUSS ORR 
%left DIVV MODD MUL ANDD
%right NOTT NEG
%nodefault
%verbose
%keyword PROGRAMM VARR INTT BOOLL READD WRITEE IFF THENN ELSEE ENDIFF WHILEE DOO ENDWHH TT FF
%arg(fileName) : string


%%
begin: PROGRAMM IDE CONS block                                              (PROG(IDE, block))                          
block : declrlist LBRACE cmds RBRACE                                        (BLK(declrlist, cmds))    
declrlist : declr declrlist                                                 (declr :: declrlist)    
          |                                                                 ([])    
declr : VARR varlist COLON typ SCOLON                                       (addDec(varlist, typ))    
typ : INTT                                                                  ("int")    
    | BOOLL                                                                 ("bool")    
varlist : variable COMMA varlist                                            (variable :: varlist)    
        | variable                                                          ([variable])    
cmds : cmdd SCOLON cmds                                                     (SEQ(cmdd, cmds))    
     |                                                                      (empty)         
cmdd : variable ASSIGN exp                                                  (funSET(variable, exp))    
     | READD variable                                                       (READ(variable))    
     | WRITEE exp                                                           (funWRITE(exp))    
     | IFF exp THENN LBRACE cmds RBRACE ELSEE LBRACE cmds RBRACE ENDIFF     (funITE(exp, cmds1, cmds2))    
     | WHILEE exp DOO LBRACE cmds RBRACE ENDWHH                             (funWH(exp, cmds))         
exp : NEG exp                      %prec NEG                                (funNEGATIVE(exp))
    | exp PLUSS exp                %prec PLUSS                              (funPLUS(exp1, exp2))
    | exp MINUSS exp               %prec MINUSS                             (funMINUS(exp1, exp2))
    | exp MUL exp                  %prec MUL                                (funTIMES(exp1, exp2))
    | exp DIVV exp                 %prec DIVV                               (funDIV(exp1, exp2))
    | exp MODD exp                 %prec MODD                               (funMOD(exp1, exp2))
    | LPAR exp RPAR                                                         (exp)
    | NOTT exp                     %prec NOTT                               (funLOGICALNOT(exp))
    | exp ORR exp                  %prec ORR                                (funOR(exp1, exp2))
    | exp ANDD exp                 %prec ANDD                               (funAND(exp1, exp2))
    | exp LTT exp                  %prec LTT                                (funLT(exp1, exp2))
    | exp LEQQ exp                 %prec LEQQ                               (funLEQ(exp1, exp2))
    | exp EQQ exp                  %prec EQQ                                (funEQ(exp1, exp2))
    | exp GTT exp                  %prec GTT                                (funGT(exp1, exp2))
    | exp GEQQ exp                 %prec GEQQ                               (funGEQ(exp1, exp2))
    | exp NEQQ exp                 %prec NEQQ                               (funNEQ(exp1, exp2))
    | variable                                                              (VAR(variable))
    | numeral                                                               (NUMBER(numeral))
    | TT                                                                    (TT)
    | FF                                                                    (FF)
variable : IDE                                                              (IDE)   
numeral : PLUSS NUM                                                         ("+"^NUM)
        | NEG NUM                                                           ("~"^NUM)
        | NUM                                                               (NUM)