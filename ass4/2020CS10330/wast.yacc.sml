functor WastLrValsFun(structure Token : TOKEN)
 : sig structure ParserData : PARSER_DATA
       structure Tokens : Wast_TOKENS
   end
 = 
struct
structure ParserData=
struct
structure Header = 
struct
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

end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\001\000\004\000\000\000\
\\001\000\001\000\014\000\000\000\
\\001\000\001\000\014\000\004\000\104\000\031\000\021\000\032\000\020\000\
\\033\000\019\000\037\000\018\000\000\000\
\\001\000\001\000\014\000\009\000\036\000\010\000\035\000\015\000\034\000\
\\017\000\033\000\026\000\032\000\040\000\031\000\041\000\030\000\000\000\
\\001\000\001\000\014\000\009\000\036\000\010\000\035\000\015\000\034\000\
\\017\000\033\000\026\000\064\000\040\000\031\000\041\000\030\000\000\000\
\\001\000\002\000\005\000\000\000\
\\001\000\003\000\096\000\000\000\
\\001\000\003\000\097\000\028\000\009\000\000\000\
\\001\000\003\000\098\000\028\000\098\000\000\000\
\\001\000\003\000\011\000\000\000\
\\001\000\003\000\067\000\000\000\
\\001\000\003\000\082\000\000\000\
\\001\000\003\000\089\000\000\000\
\\001\000\004\000\103\000\000\000\
\\001\000\004\000\026\000\000\000\
\\001\000\004\000\085\000\000\000\
\\001\000\004\000\086\000\000\000\
\\001\000\004\000\091\000\000\000\
\\001\000\005\000\101\000\000\000\
\\001\000\005\000\102\000\007\000\022\000\000\000\
\\001\000\005\000\130\000\006\000\130\000\007\000\130\000\008\000\130\000\
\\010\000\130\000\011\000\130\000\012\000\130\000\013\000\130\000\
\\014\000\130\000\016\000\130\000\018\000\130\000\019\000\130\000\
\\020\000\130\000\021\000\130\000\022\000\130\000\023\000\130\000\
\\024\000\130\000\025\000\130\000\034\000\130\000\038\000\130\000\000\000\
\\001\000\005\000\023\000\000\000\
\\001\000\006\000\099\000\000\000\
\\001\000\006\000\100\000\000\000\
\\001\000\006\000\105\000\010\000\059\000\011\000\058\000\012\000\057\000\
\\013\000\056\000\014\000\055\000\018\000\054\000\019\000\053\000\
\\020\000\052\000\021\000\051\000\022\000\050\000\023\000\049\000\
\\024\000\048\000\025\000\047\000\000\000\
\\001\000\006\000\106\000\000\000\
\\001\000\006\000\107\000\010\000\059\000\011\000\058\000\012\000\057\000\
\\013\000\056\000\014\000\055\000\018\000\054\000\019\000\053\000\
\\020\000\052\000\021\000\051\000\022\000\050\000\023\000\049\000\
\\024\000\048\000\025\000\047\000\000\000\
\\001\000\006\000\108\000\000\000\
\\001\000\006\000\109\000\000\000\
\\001\000\006\000\110\000\010\000\110\000\011\000\110\000\012\000\110\000\
\\013\000\110\000\014\000\110\000\016\000\110\000\018\000\110\000\
\\019\000\110\000\020\000\110\000\021\000\110\000\022\000\110\000\
\\023\000\110\000\024\000\110\000\025\000\110\000\034\000\110\000\
\\038\000\110\000\000\000\
\\001\000\006\000\111\000\010\000\111\000\011\000\111\000\012\000\057\000\
\\013\000\056\000\014\000\055\000\016\000\111\000\018\000\111\000\
\\019\000\053\000\020\000\111\000\021\000\111\000\022\000\111\000\
\\023\000\111\000\024\000\111\000\025\000\111\000\034\000\111\000\
\\038\000\111\000\000\000\
\\001\000\006\000\112\000\010\000\112\000\011\000\112\000\012\000\057\000\
\\013\000\056\000\014\000\055\000\016\000\112\000\018\000\112\000\
\\019\000\053\000\020\000\112\000\021\000\112\000\022\000\112\000\
\\023\000\112\000\024\000\112\000\025\000\112\000\034\000\112\000\
\\038\000\112\000\000\000\
\\001\000\006\000\113\000\010\000\113\000\011\000\113\000\012\000\113\000\
\\013\000\113\000\014\000\113\000\016\000\113\000\018\000\113\000\
\\019\000\113\000\020\000\113\000\021\000\113\000\022\000\113\000\
\\023\000\113\000\024\000\113\000\025\000\113\000\034\000\113\000\
\\038\000\113\000\000\000\
\\001\000\006\000\114\000\010\000\114\000\011\000\114\000\012\000\114\000\
\\013\000\114\000\014\000\114\000\016\000\114\000\018\000\114\000\
\\019\000\114\000\020\000\114\000\021\000\114\000\022\000\114\000\
\\023\000\114\000\024\000\114\000\025\000\114\000\034\000\114\000\
\\038\000\114\000\000\000\
\\001\000\006\000\115\000\010\000\115\000\011\000\115\000\012\000\115\000\
\\013\000\115\000\014\000\115\000\016\000\115\000\018\000\115\000\
\\019\000\115\000\020\000\115\000\021\000\115\000\022\000\115\000\
\\023\000\115\000\024\000\115\000\025\000\115\000\034\000\115\000\
\\038\000\115\000\000\000\
\\001\000\006\000\116\000\010\000\116\000\011\000\116\000\012\000\116\000\
\\013\000\116\000\014\000\116\000\016\000\116\000\018\000\116\000\
\\019\000\116\000\020\000\116\000\021\000\116\000\022\000\116\000\
\\023\000\116\000\024\000\116\000\025\000\116\000\034\000\116\000\
\\038\000\116\000\000\000\
\\001\000\006\000\117\000\010\000\117\000\011\000\117\000\012\000\117\000\
\\013\000\117\000\014\000\117\000\016\000\117\000\018\000\117\000\
\\019\000\117\000\020\000\117\000\021\000\117\000\022\000\117\000\
\\023\000\117\000\024\000\117\000\025\000\117\000\034\000\117\000\
\\038\000\117\000\000\000\
\\001\000\006\000\118\000\010\000\118\000\011\000\118\000\012\000\057\000\
\\013\000\056\000\014\000\055\000\016\000\118\000\018\000\118\000\
\\019\000\053\000\020\000\118\000\021\000\118\000\022\000\118\000\
\\023\000\118\000\024\000\118\000\025\000\118\000\034\000\118\000\
\\038\000\118\000\000\000\
\\001\000\006\000\119\000\010\000\119\000\011\000\119\000\012\000\119\000\
\\013\000\119\000\014\000\119\000\016\000\119\000\018\000\119\000\
\\019\000\119\000\020\000\119\000\021\000\119\000\022\000\119\000\
\\023\000\119\000\024\000\119\000\025\000\119\000\034\000\119\000\
\\038\000\119\000\000\000\
\\001\000\006\000\120\000\010\000\059\000\011\000\058\000\012\000\057\000\
\\013\000\056\000\014\000\055\000\016\000\120\000\018\000\054\000\
\\019\000\053\000\020\000\120\000\021\000\120\000\022\000\120\000\
\\023\000\120\000\024\000\120\000\025\000\120\000\034\000\120\000\
\\038\000\120\000\000\000\
\\001\000\006\000\121\000\010\000\059\000\011\000\058\000\012\000\057\000\
\\013\000\056\000\014\000\055\000\016\000\121\000\018\000\054\000\
\\019\000\053\000\020\000\121\000\021\000\121\000\022\000\121\000\
\\023\000\121\000\024\000\121\000\025\000\121\000\034\000\121\000\
\\038\000\121\000\000\000\
\\001\000\006\000\122\000\010\000\059\000\011\000\058\000\012\000\057\000\
\\013\000\056\000\014\000\055\000\016\000\122\000\018\000\054\000\
\\019\000\053\000\020\000\122\000\021\000\122\000\022\000\122\000\
\\023\000\122\000\024\000\122\000\025\000\122\000\034\000\122\000\
\\038\000\122\000\000\000\
\\001\000\006\000\123\000\010\000\059\000\011\000\058\000\012\000\057\000\
\\013\000\056\000\014\000\055\000\016\000\123\000\018\000\054\000\
\\019\000\053\000\020\000\123\000\021\000\123\000\022\000\123\000\
\\023\000\123\000\024\000\123\000\025\000\123\000\034\000\123\000\
\\038\000\123\000\000\000\
\\001\000\006\000\124\000\010\000\059\000\011\000\058\000\012\000\057\000\
\\013\000\056\000\014\000\055\000\016\000\124\000\018\000\054\000\
\\019\000\053\000\020\000\124\000\021\000\124\000\022\000\124\000\
\\023\000\124\000\024\000\124\000\025\000\124\000\034\000\124\000\
\\038\000\124\000\000\000\
\\001\000\006\000\125\000\010\000\059\000\011\000\058\000\012\000\057\000\
\\013\000\056\000\014\000\055\000\016\000\125\000\018\000\054\000\
\\019\000\053\000\020\000\125\000\021\000\125\000\022\000\125\000\
\\023\000\125\000\024\000\125\000\025\000\125\000\034\000\125\000\
\\038\000\125\000\000\000\
\\001\000\006\000\126\000\010\000\126\000\011\000\126\000\012\000\126\000\
\\013\000\126\000\014\000\126\000\016\000\126\000\018\000\126\000\
\\019\000\126\000\020\000\126\000\021\000\126\000\022\000\126\000\
\\023\000\126\000\024\000\126\000\025\000\126\000\034\000\126\000\
\\038\000\126\000\000\000\
\\001\000\006\000\127\000\010\000\127\000\011\000\127\000\012\000\127\000\
\\013\000\127\000\014\000\127\000\016\000\127\000\018\000\127\000\
\\019\000\127\000\020\000\127\000\021\000\127\000\022\000\127\000\
\\023\000\127\000\024\000\127\000\025\000\127\000\034\000\127\000\
\\038\000\127\000\000\000\
\\001\000\006\000\128\000\010\000\128\000\011\000\128\000\012\000\128\000\
\\013\000\128\000\014\000\128\000\016\000\128\000\018\000\128\000\
\\019\000\128\000\020\000\128\000\021\000\128\000\022\000\128\000\
\\023\000\128\000\024\000\128\000\025\000\128\000\034\000\128\000\
\\038\000\128\000\000\000\
\\001\000\006\000\129\000\010\000\129\000\011\000\129\000\012\000\129\000\
\\013\000\129\000\014\000\129\000\016\000\129\000\018\000\129\000\
\\019\000\129\000\020\000\129\000\021\000\129\000\022\000\129\000\
\\023\000\129\000\024\000\129\000\025\000\129\000\034\000\129\000\
\\038\000\129\000\000\000\
\\001\000\006\000\131\000\010\000\131\000\011\000\131\000\012\000\131\000\
\\013\000\131\000\014\000\131\000\016\000\131\000\018\000\131\000\
\\019\000\131\000\020\000\131\000\021\000\131\000\022\000\131\000\
\\023\000\131\000\024\000\131\000\025\000\131\000\034\000\131\000\
\\038\000\131\000\000\000\
\\001\000\006\000\132\000\010\000\132\000\011\000\132\000\012\000\132\000\
\\013\000\132\000\014\000\132\000\016\000\132\000\018\000\132\000\
\\019\000\132\000\020\000\132\000\021\000\132\000\022\000\132\000\
\\023\000\132\000\024\000\132\000\025\000\132\000\034\000\132\000\
\\038\000\132\000\000\000\
\\001\000\006\000\133\000\010\000\133\000\011\000\133\000\012\000\133\000\
\\013\000\133\000\014\000\133\000\016\000\133\000\018\000\133\000\
\\019\000\133\000\020\000\133\000\021\000\133\000\022\000\133\000\
\\023\000\133\000\024\000\133\000\025\000\133\000\034\000\133\000\
\\038\000\133\000\000\000\
\\001\000\006\000\025\000\000\000\
\\001\000\006\000\066\000\000\000\
\\001\000\008\000\024\000\000\000\
\\001\000\010\000\059\000\011\000\058\000\012\000\057\000\013\000\056\000\
\\014\000\055\000\016\000\081\000\018\000\054\000\019\000\053\000\
\\020\000\052\000\021\000\051\000\022\000\050\000\023\000\049\000\
\\024\000\048\000\025\000\047\000\000\000\
\\001\000\010\000\059\000\011\000\058\000\012\000\057\000\013\000\056\000\
\\014\000\055\000\018\000\054\000\019\000\053\000\020\000\052\000\
\\021\000\051\000\022\000\050\000\023\000\049\000\024\000\048\000\
\\025\000\047\000\034\000\065\000\000\000\
\\001\000\010\000\059\000\011\000\058\000\012\000\057\000\013\000\056\000\
\\014\000\055\000\018\000\054\000\019\000\053\000\020\000\052\000\
\\021\000\051\000\022\000\050\000\023\000\049\000\024\000\048\000\
\\025\000\047\000\038\000\046\000\000\000\
\\001\000\026\000\062\000\000\000\
\\001\000\027\000\003\000\000\000\
\\001\000\029\000\043\000\030\000\042\000\000\000\
\\001\000\035\000\088\000\000\000\
\\001\000\036\000\092\000\000\000\
\\001\000\039\000\087\000\000\000\
\\001\000\043\000\000\000\000\000\
\\001\000\043\000\094\000\000\000\
\\001\000\043\000\095\000\000\000\
\"
val actionRowNumbers =
"\059\000\000\000\005\000\007\000\
\\007\000\009\000\065\000\001\000\
\\006\000\002\000\019\000\021\000\
\\020\000\054\000\052\000\014\000\
\\003\000\003\000\003\000\001\000\
\\001\000\060\000\003\000\002\000\
\\066\000\046\000\045\000\057\000\
\\048\000\047\000\051\000\003\000\
\\003\000\058\000\004\000\056\000\
\\026\000\025\000\018\000\053\000\
\\023\000\022\000\024\000\013\000\
\\010\000\003\000\003\000\003\000\
\\003\000\003\000\003\000\003\000\
\\003\000\003\000\003\000\003\000\
\\003\000\003\000\036\000\055\000\
\\049\000\029\000\050\000\011\000\
\\008\000\002\000\044\000\043\000\
\\042\000\041\000\040\000\039\000\
\\038\000\037\000\034\000\033\000\
\\032\000\031\000\030\000\035\000\
\\002\000\015\000\016\000\063\000\
\\061\000\028\000\012\000\002\000\
\\017\000\062\000\027\000\064\000"
val gotoT =
"\
\\001\000\091\000\000\000\
\\000\000\
\\000\000\
\\002\000\006\000\003\000\005\000\004\000\004\000\000\000\
\\003\000\008\000\004\000\004\000\000\000\
\\000\000\
\\000\000\
\\006\000\011\000\010\000\010\000\000\000\
\\000\000\
\\007\000\015\000\008\000\014\000\010\000\013\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\009\000\027\000\010\000\026\000\011\000\025\000\000\000\
\\009\000\035\000\010\000\026\000\011\000\025\000\000\000\
\\009\000\036\000\010\000\026\000\011\000\025\000\000\000\
\\010\000\037\000\000\000\
\\006\000\038\000\010\000\010\000\000\000\
\\005\000\039\000\000\000\
\\009\000\042\000\010\000\026\000\011\000\025\000\000\000\
\\007\000\043\000\008\000\014\000\010\000\013\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\009\000\058\000\010\000\026\000\011\000\025\000\000\000\
\\009\000\059\000\010\000\026\000\011\000\025\000\000\000\
\\000\000\
\\009\000\061\000\010\000\026\000\011\000\025\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\009\000\066\000\010\000\026\000\011\000\025\000\000\000\
\\009\000\067\000\010\000\026\000\011\000\025\000\000\000\
\\009\000\068\000\010\000\026\000\011\000\025\000\000\000\
\\009\000\069\000\010\000\026\000\011\000\025\000\000\000\
\\009\000\070\000\010\000\026\000\011\000\025\000\000\000\
\\009\000\071\000\010\000\026\000\011\000\025\000\000\000\
\\009\000\072\000\010\000\026\000\011\000\025\000\000\000\
\\009\000\073\000\010\000\026\000\011\000\025\000\000\000\
\\009\000\074\000\010\000\026\000\011\000\025\000\000\000\
\\009\000\075\000\010\000\026\000\011\000\025\000\000\000\
\\009\000\076\000\010\000\026\000\011\000\025\000\000\000\
\\009\000\077\000\010\000\026\000\011\000\025\000\000\000\
\\009\000\078\000\010\000\026\000\011\000\025\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\007\000\081\000\008\000\014\000\010\000\013\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\007\000\082\000\008\000\014\000\010\000\013\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\007\000\088\000\008\000\014\000\010\000\013\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\"
val numstates = 92
val numrules = 40
val s = ref "" and index = ref 0
val string_to_int = fn () => 
let val i = !index
in index := i+2; Char.ord(String.sub(!s,i)) + Char.ord(String.sub(!s,i+1)) * 256
end
val string_to_list = fn s' =>
    let val len = String.size s'
        fun f () =
           if !index < len then string_to_int() :: f()
           else nil
   in index := 0; s := s'; f ()
   end
val string_to_pairlist = fn (conv_key,conv_entry) =>
     let fun f () =
         case string_to_int()
         of 0 => EMPTY
          | n => PAIR(conv_key (n-1),conv_entry (string_to_int()),f())
     in f
     end
val string_to_pairlist_default = fn (conv_key,conv_entry) =>
    let val conv_row = string_to_pairlist(conv_key,conv_entry)
    in fn () =>
       let val default = conv_entry(string_to_int())
           val row = conv_row()
       in (row,default)
       end
   end
val string_to_table = fn (convert_row,s') =>
    let val len = String.size s'
        fun f ()=
           if !index < len then convert_row() :: f()
           else nil
     in (s := s'; index := 0; f ())
     end
local
  val memo = Array.array(numstates+numrules,ERROR)
  val _ =let fun g i=(Array.update(memo,i,REDUCE(i-numstates)); g(i+1))
       fun f i =
            if i=numstates then g i
            else (Array.update(memo,i,SHIFT (STATE i)); f (i+1))
          in f 0 handle General.Subscript => ()
          end
in
val entry_to_action = fn 0 => ACCEPT | 1 => ERROR | j => Array.sub(memo,(j-2))
end
val gotoT=Array.fromList(string_to_table(string_to_pairlist(NT,STATE),gotoT))
val actionRows=string_to_table(string_to_pairlist_default(T,entry_to_action),actionRows)
val actionRowNumbers = string_to_list actionRowNumbers
val actionT = let val actionRowLookUp=
let val a=Array.fromList(actionRows) in fn i=>Array.sub(a,i) end
in Array.fromList(List.map actionRowLookUp actionRowNumbers)
end
in LrTable.mkLrTable {actions=actionT,gotos=gotoT,numRules=numrules,
numStates=numstates,initialState=STATE 0}
end
end
local open Header in
type pos = int
type arg = string
structure MlyValue = 
struct
datatype svalue = VOID | ntVOID of unit ->  unit
 | NUM of unit ->  (string) | IDE of unit ->  (string)
 | numeral of unit ->  (Numeral) | variable of unit ->  (Variable)
 | exp of unit ->  (Exp) | cmdd of unit ->  (Cmd)
 | cmds of unit ->  (CmdSeq) | varlist of unit ->  (Variable list)
 | typ of unit ->  (Typ) | declr of unit ->  (Declr)
 | declrlist of unit ->  (Declr list) | block of unit ->  (Block)
 | begin of unit ->  (Ast)
end
type svalue = MlyValue.svalue
type result = Ast
end
structure EC=
struct
open LrTable
infix 5 $$
fun x $$ y = y::x
val is_keyword =
fn (T 26) => true | (T 27) => true | (T 28) => true | (T 29) => true
 | (T 30) => true | (T 31) => true | (T 32) => true | (T 33) => true
 | (T 34) => true | (T 35) => true | (T 36) => true | (T 37) => true
 | (T 38) => true | (T 39) => true | (T 40) => true | _ => false
val preferred_change : (term list * term list) list = 
nil
val noShift = 
fn (T 42) => true | _ => false
val showTerminal =
fn (T 0) => "IDE"
  | (T 1) => "CONS"
  | (T 2) => "LBRACE"
  | (T 3) => "RBRACE"
  | (T 4) => "COLON"
  | (T 5) => "SCOLON"
  | (T 6) => "COMMA"
  | (T 7) => "ASSIGN"
  | (T 8) => "NEG"
  | (T 9) => "PLUSS"
  | (T 10) => "MINUSS"
  | (T 11) => "MUL"
  | (T 12) => "DIVV"
  | (T 13) => "MODD"
  | (T 14) => "LPAR"
  | (T 15) => "RPAR"
  | (T 16) => "NOTT"
  | (T 17) => "ORR"
  | (T 18) => "ANDD"
  | (T 19) => "LTT"
  | (T 20) => "LEQQ"
  | (T 21) => "EQQ"
  | (T 22) => "GTT"
  | (T 23) => "GEQQ"
  | (T 24) => "NEQQ"
  | (T 25) => "NUM"
  | (T 26) => "PROGRAMM"
  | (T 27) => "VARR"
  | (T 28) => "INTT"
  | (T 29) => "BOOLL"
  | (T 30) => "READD"
  | (T 31) => "WRITEE"
  | (T 32) => "IFF"
  | (T 33) => "THENN"
  | (T 34) => "ELSEE"
  | (T 35) => "ENDIFF"
  | (T 36) => "WHILEE"
  | (T 37) => "DOO"
  | (T 38) => "ENDWHH"
  | (T 39) => "TT"
  | (T 40) => "FF"
  | (T 41) => "BADCH"
  | (T 42) => "EOF"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn _ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 42) $$ (T 41) $$ (T 40) $$ (T 39) $$ (T 38) $$ (T 37) $$ (T 36)
 $$ (T 35) $$ (T 34) $$ (T 33) $$ (T 32) $$ (T 31) $$ (T 30) $$ (T 29)
 $$ (T 28) $$ (T 27) $$ (T 26) $$ (T 24) $$ (T 23) $$ (T 22) $$ (T 21)
 $$ (T 20) $$ (T 19) $$ (T 18) $$ (T 17) $$ (T 16) $$ (T 15) $$ (T 14)
 $$ (T 13) $$ (T 12) $$ (T 11) $$ (T 10) $$ (T 9) $$ (T 8) $$ (T 7)
 $$ (T 6) $$ (T 5) $$ (T 4) $$ (T 3) $$ (T 2) $$ (T 1)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (fileName):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( MlyValue.block block1, _, block1right)) :: _ :: ( _,
 ( MlyValue.IDE IDE1, _, _)) :: ( _, ( _, PROGRAMM1left, _)) :: 
rest671)) => let val  result = MlyValue.begin (fn _ => let val  (IDE
 as IDE1) = IDE1 ()
 val  (block as block1) = block1 ()
 in (PROG(IDE, block))
end)
 in ( LrTable.NT 0, ( result, PROGRAMM1left, block1right), rest671)

end
|  ( 1, ( ( _, ( _, _, RBRACE1right)) :: ( _, ( MlyValue.cmds cmds1, _
, _)) :: _ :: ( _, ( MlyValue.declrlist declrlist1, declrlist1left, _)
) :: rest671)) => let val  result = MlyValue.block (fn _ => let val  (
declrlist as declrlist1) = declrlist1 ()
 val  (cmds as cmds1) = cmds1 ()
 in (BLK(declrlist, cmds))
end)
 in ( LrTable.NT 1, ( result, declrlist1left, RBRACE1right), rest671)

end
|  ( 2, ( ( _, ( MlyValue.declrlist declrlist1, _, declrlist1right))
 :: ( _, ( MlyValue.declr declr1, declr1left, _)) :: rest671)) => let
 val  result = MlyValue.declrlist (fn _ => let val  (declr as declr1)
 = declr1 ()
 val  (declrlist as declrlist1) = declrlist1 ()
 in (declr :: declrlist)
end)
 in ( LrTable.NT 2, ( result, declr1left, declrlist1right), rest671)

end
|  ( 3, ( rest671)) => let val  result = MlyValue.declrlist (fn _ => (
[]))
 in ( LrTable.NT 2, ( result, defaultPos, defaultPos), rest671)
end
|  ( 4, ( ( _, ( _, _, SCOLON1right)) :: ( _, ( MlyValue.typ typ1, _,
 _)) :: _ :: ( _, ( MlyValue.varlist varlist1, _, _)) :: ( _, ( _, 
VARR1left, _)) :: rest671)) => let val  result = MlyValue.declr (fn _
 => let val  (varlist as varlist1) = varlist1 ()
 val  (typ as typ1) = typ1 ()
 in (addDec(varlist, typ))
end)
 in ( LrTable.NT 3, ( result, VARR1left, SCOLON1right), rest671)
end
|  ( 5, ( ( _, ( _, INTT1left, INTT1right)) :: rest671)) => let val  
result = MlyValue.typ (fn _ => ("int"))
 in ( LrTable.NT 4, ( result, INTT1left, INTT1right), rest671)
end
|  ( 6, ( ( _, ( _, BOOLL1left, BOOLL1right)) :: rest671)) => let val 
 result = MlyValue.typ (fn _ => ("bool"))
 in ( LrTable.NT 4, ( result, BOOLL1left, BOOLL1right), rest671)
end
|  ( 7, ( ( _, ( MlyValue.varlist varlist1, _, varlist1right)) :: _ ::
 ( _, ( MlyValue.variable variable1, variable1left, _)) :: rest671))
 => let val  result = MlyValue.varlist (fn _ => let val  (variable as 
variable1) = variable1 ()
 val  (varlist as varlist1) = varlist1 ()
 in (variable :: varlist)
end)
 in ( LrTable.NT 5, ( result, variable1left, varlist1right), rest671)

end
|  ( 8, ( ( _, ( MlyValue.variable variable1, variable1left, 
variable1right)) :: rest671)) => let val  result = MlyValue.varlist
 (fn _ => let val  (variable as variable1) = variable1 ()
 in ([variable])
end)
 in ( LrTable.NT 5, ( result, variable1left, variable1right), rest671)

end
|  ( 9, ( ( _, ( MlyValue.cmds cmds1, _, cmds1right)) :: _ :: ( _, ( 
MlyValue.cmdd cmdd1, cmdd1left, _)) :: rest671)) => let val  result = 
MlyValue.cmds (fn _ => let val  (cmdd as cmdd1) = cmdd1 ()
 val  (cmds as cmds1) = cmds1 ()
 in (SEQ(cmdd, cmds))
end)
 in ( LrTable.NT 6, ( result, cmdd1left, cmds1right), rest671)
end
|  ( 10, ( rest671)) => let val  result = MlyValue.cmds (fn _ => (
empty))
 in ( LrTable.NT 6, ( result, defaultPos, defaultPos), rest671)
end
|  ( 11, ( ( _, ( MlyValue.exp exp1, _, exp1right)) :: _ :: ( _, ( 
MlyValue.variable variable1, variable1left, _)) :: rest671)) => let
 val  result = MlyValue.cmdd (fn _ => let val  (variable as variable1)
 = variable1 ()
 val  (exp as exp1) = exp1 ()
 in (funSET(variable, exp))
end)
 in ( LrTable.NT 7, ( result, variable1left, exp1right), rest671)
end
|  ( 12, ( ( _, ( MlyValue.variable variable1, _, variable1right)) :: 
( _, ( _, READD1left, _)) :: rest671)) => let val  result = 
MlyValue.cmdd (fn _ => let val  (variable as variable1) = variable1 ()
 in (READ(variable))
end)
 in ( LrTable.NT 7, ( result, READD1left, variable1right), rest671)

end
|  ( 13, ( ( _, ( MlyValue.exp exp1, _, exp1right)) :: ( _, ( _, 
WRITEE1left, _)) :: rest671)) => let val  result = MlyValue.cmdd (fn _
 => let val  (exp as exp1) = exp1 ()
 in (funWRITE(exp))
end)
 in ( LrTable.NT 7, ( result, WRITEE1left, exp1right), rest671)
end
|  ( 14, ( ( _, ( _, _, ENDIFF1right)) :: _ :: ( _, ( MlyValue.cmds 
cmds2, _, _)) :: _ :: _ :: _ :: ( _, ( MlyValue.cmds cmds1, _, _)) ::
 _ :: _ :: ( _, ( MlyValue.exp exp1, _, _)) :: ( _, ( _, IFF1left, _))
 :: rest671)) => let val  result = MlyValue.cmdd (fn _ => let val  (
exp as exp1) = exp1 ()
 val  cmds1 = cmds1 ()
 val  cmds2 = cmds2 ()
 in (funITE(exp, cmds1, cmds2))
end)
 in ( LrTable.NT 7, ( result, IFF1left, ENDIFF1right), rest671)
end
|  ( 15, ( ( _, ( _, _, ENDWHH1right)) :: _ :: ( _, ( MlyValue.cmds 
cmds1, _, _)) :: _ :: _ :: ( _, ( MlyValue.exp exp1, _, _)) :: ( _, (
 _, WHILEE1left, _)) :: rest671)) => let val  result = MlyValue.cmdd
 (fn _ => let val  (exp as exp1) = exp1 ()
 val  (cmds as cmds1) = cmds1 ()
 in (funWH(exp, cmds))
end)
 in ( LrTable.NT 7, ( result, WHILEE1left, ENDWHH1right), rest671)
end
|  ( 16, ( ( _, ( MlyValue.exp exp1, _, exp1right)) :: ( _, ( _, 
NEG1left, _)) :: rest671)) => let val  result = MlyValue.exp (fn _ =>
 let val  (exp as exp1) = exp1 ()
 in (funNEGATIVE(exp))
end)
 in ( LrTable.NT 8, ( result, NEG1left, exp1right), rest671)
end
|  ( 17, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (funPLUS(exp1, exp2))
end)
 in ( LrTable.NT 8, ( result, exp1left, exp2right), rest671)
end
|  ( 18, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (funMINUS(exp1, exp2))
end)
 in ( LrTable.NT 8, ( result, exp1left, exp2right), rest671)
end
|  ( 19, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (funTIMES(exp1, exp2))
end)
 in ( LrTable.NT 8, ( result, exp1left, exp2right), rest671)
end
|  ( 20, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (funDIV(exp1, exp2))
end)
 in ( LrTable.NT 8, ( result, exp1left, exp2right), rest671)
end
|  ( 21, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (funMOD(exp1, exp2))
end)
 in ( LrTable.NT 8, ( result, exp1left, exp2right), rest671)
end
|  ( 22, ( ( _, ( _, _, RPAR1right)) :: ( _, ( MlyValue.exp exp1, _, _
)) :: ( _, ( _, LPAR1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  (exp as exp1) = exp1 ()
 in (exp)
end)
 in ( LrTable.NT 8, ( result, LPAR1left, RPAR1right), rest671)
end
|  ( 23, ( ( _, ( MlyValue.exp exp1, _, exp1right)) :: ( _, ( _, 
NOTT1left, _)) :: rest671)) => let val  result = MlyValue.exp (fn _ =>
 let val  (exp as exp1) = exp1 ()
 in (funLOGICALNOT(exp))
end)
 in ( LrTable.NT 8, ( result, NOTT1left, exp1right), rest671)
end
|  ( 24, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (funOR(exp1, exp2))
end)
 in ( LrTable.NT 8, ( result, exp1left, exp2right), rest671)
end
|  ( 25, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (funAND(exp1, exp2))
end)
 in ( LrTable.NT 8, ( result, exp1left, exp2right), rest671)
end
|  ( 26, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (funLT(exp1, exp2))
end)
 in ( LrTable.NT 8, ( result, exp1left, exp2right), rest671)
end
|  ( 27, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (funLEQ(exp1, exp2))
end)
 in ( LrTable.NT 8, ( result, exp1left, exp2right), rest671)
end
|  ( 28, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (funEQ(exp1, exp2))
end)
 in ( LrTable.NT 8, ( result, exp1left, exp2right), rest671)
end
|  ( 29, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (funGT(exp1, exp2))
end)
 in ( LrTable.NT 8, ( result, exp1left, exp2right), rest671)
end
|  ( 30, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (funGEQ(exp1, exp2))
end)
 in ( LrTable.NT 8, ( result, exp1left, exp2right), rest671)
end
|  ( 31, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (funNEQ(exp1, exp2))
end)
 in ( LrTable.NT 8, ( result, exp1left, exp2right), rest671)
end
|  ( 32, ( ( _, ( MlyValue.variable variable1, variable1left, 
variable1right)) :: rest671)) => let val  result = MlyValue.exp (fn _
 => let val  (variable as variable1) = variable1 ()
 in (VAR(variable))
end)
 in ( LrTable.NT 8, ( result, variable1left, variable1right), rest671)

end
|  ( 33, ( ( _, ( MlyValue.numeral numeral1, numeral1left, 
numeral1right)) :: rest671)) => let val  result = MlyValue.exp (fn _
 => let val  (numeral as numeral1) = numeral1 ()
 in (NUMBER(numeral))
end)
 in ( LrTable.NT 8, ( result, numeral1left, numeral1right), rest671)

end
|  ( 34, ( ( _, ( _, TT1left, TT1right)) :: rest671)) => let val  
result = MlyValue.exp (fn _ => (TT))
 in ( LrTable.NT 8, ( result, TT1left, TT1right), rest671)
end
|  ( 35, ( ( _, ( _, FF1left, FF1right)) :: rest671)) => let val  
result = MlyValue.exp (fn _ => (FF))
 in ( LrTable.NT 8, ( result, FF1left, FF1right), rest671)
end
|  ( 36, ( ( _, ( MlyValue.IDE IDE1, IDE1left, IDE1right)) :: rest671)
) => let val  result = MlyValue.variable (fn _ => let val  (IDE as 
IDE1) = IDE1 ()
 in (IDE)
end)
 in ( LrTable.NT 9, ( result, IDE1left, IDE1right), rest671)
end
|  ( 37, ( ( _, ( MlyValue.NUM NUM1, _, NUM1right)) :: ( _, ( _, 
PLUSS1left, _)) :: rest671)) => let val  result = MlyValue.numeral (fn
 _ => let val  (NUM as NUM1) = NUM1 ()
 in ("+"^NUM)
end)
 in ( LrTable.NT 10, ( result, PLUSS1left, NUM1right), rest671)
end
|  ( 38, ( ( _, ( MlyValue.NUM NUM1, _, NUM1right)) :: ( _, ( _, 
NEG1left, _)) :: rest671)) => let val  result = MlyValue.numeral (fn _
 => let val  (NUM as NUM1) = NUM1 ()
 in ("~"^NUM)
end)
 in ( LrTable.NT 10, ( result, NEG1left, NUM1right), rest671)
end
|  ( 39, ( ( _, ( MlyValue.NUM NUM1, NUM1left, NUM1right)) :: rest671)
) => let val  result = MlyValue.numeral (fn _ => let val  (NUM as NUM1
) = NUM1 ()
 in (NUM)
end)
 in ( LrTable.NT 10, ( result, NUM1left, NUM1right), rest671)
end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.begin x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a ()
end
end
structure Tokens : Wast_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun IDE (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(
ParserData.MlyValue.IDE (fn () => i),p1,p2))
fun CONS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(
ParserData.MlyValue.VOID,p1,p2))
fun LBRACE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(
ParserData.MlyValue.VOID,p1,p2))
fun RBRACE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(
ParserData.MlyValue.VOID,p1,p2))
fun COLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(
ParserData.MlyValue.VOID,p1,p2))
fun SCOLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(
ParserData.MlyValue.VOID,p1,p2))
fun COMMA (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(
ParserData.MlyValue.VOID,p1,p2))
fun ASSIGN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(
ParserData.MlyValue.VOID,p1,p2))
fun NEG (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(
ParserData.MlyValue.VOID,p1,p2))
fun PLUSS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(
ParserData.MlyValue.VOID,p1,p2))
fun MINUSS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(
ParserData.MlyValue.VOID,p1,p2))
fun MUL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(
ParserData.MlyValue.VOID,p1,p2))
fun DIVV (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(
ParserData.MlyValue.VOID,p1,p2))
fun MODD (p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(
ParserData.MlyValue.VOID,p1,p2))
fun LPAR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(
ParserData.MlyValue.VOID,p1,p2))
fun RPAR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 15,(
ParserData.MlyValue.VOID,p1,p2))
fun NOTT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 16,(
ParserData.MlyValue.VOID,p1,p2))
fun ORR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 17,(
ParserData.MlyValue.VOID,p1,p2))
fun ANDD (p1,p2) = Token.TOKEN (ParserData.LrTable.T 18,(
ParserData.MlyValue.VOID,p1,p2))
fun LTT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 19,(
ParserData.MlyValue.VOID,p1,p2))
fun LEQQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 20,(
ParserData.MlyValue.VOID,p1,p2))
fun EQQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 21,(
ParserData.MlyValue.VOID,p1,p2))
fun GTT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 22,(
ParserData.MlyValue.VOID,p1,p2))
fun GEQQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 23,(
ParserData.MlyValue.VOID,p1,p2))
fun NEQQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 24,(
ParserData.MlyValue.VOID,p1,p2))
fun NUM (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 25,(
ParserData.MlyValue.NUM (fn () => i),p1,p2))
fun PROGRAMM (p1,p2) = Token.TOKEN (ParserData.LrTable.T 26,(
ParserData.MlyValue.VOID,p1,p2))
fun VARR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 27,(
ParserData.MlyValue.VOID,p1,p2))
fun INTT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 28,(
ParserData.MlyValue.VOID,p1,p2))
fun BOOLL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 29,(
ParserData.MlyValue.VOID,p1,p2))
fun READD (p1,p2) = Token.TOKEN (ParserData.LrTable.T 30,(
ParserData.MlyValue.VOID,p1,p2))
fun WRITEE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 31,(
ParserData.MlyValue.VOID,p1,p2))
fun IFF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 32,(
ParserData.MlyValue.VOID,p1,p2))
fun THENN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 33,(
ParserData.MlyValue.VOID,p1,p2))
fun ELSEE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 34,(
ParserData.MlyValue.VOID,p1,p2))
fun ENDIFF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 35,(
ParserData.MlyValue.VOID,p1,p2))
fun WHILEE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 36,(
ParserData.MlyValue.VOID,p1,p2))
fun DOO (p1,p2) = Token.TOKEN (ParserData.LrTable.T 37,(
ParserData.MlyValue.VOID,p1,p2))
fun ENDWHH (p1,p2) = Token.TOKEN (ParserData.LrTable.T 38,(
ParserData.MlyValue.VOID,p1,p2))
fun TT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 39,(
ParserData.MlyValue.VOID,p1,p2))
fun FF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 40,(
ParserData.MlyValue.VOID,p1,p2))
fun BADCH (p1,p2) = Token.TOKEN (ParserData.LrTable.T 41,(
ParserData.MlyValue.VOID,p1,p2))
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 42,(
ParserData.MlyValue.VOID,p1,p2))
end
end
