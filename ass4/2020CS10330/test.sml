signature STACK =
sig
    type 'a Stack
    exception EmptyStack
    exception StackTooSmall
    exception Error of string
    val create: 'a Stack
    val push: 'a * 'a Stack -> 'a Stack
    val pop: 'a Stack -> 'a Stack
    val top: 'a Stack -> 'a
    val empty: 'a Stack -> bool
    val poptop: 'a Stack -> ('a * 'a Stack) option
    val nth: 'a Stack * int -> 'a
    val drop: 'a Stack * int -> 'a Stack
    val depth: 'a Stack -> int
    val app: ('a -> unit) -> 'a Stack -> unit
    val map: ('a -> 'b) -> 'a Stack -> 'b Stack
    val mapPartial: ('a -> 'b option) -> 'a Stack -> 'b Stack
    val find: ('a -> bool) -> 'a Stack -> 'a option
    val filter: ('a -> bool) -> 'a Stack -> 'a Stack
    val foldr: ('a * 'b -> 'b) -> 'b -> 'a Stack -> 'b
    val foldl: ('a * 'b -> 'b) -> 'b -> 'a Stack -> 'b
    val exists: ('a -> bool) -> 'a Stack -> bool
    val all: ('a -> bool) -> 'a Stack -> bool
    val list2stack: 'a list -> 'a Stack 
    val stack2list: 'a Stack -> 'a list
    val toString: ('a -> string) -> 'a Stack -> string
end

(*The functions of the stack signature are implemented using the inbuilt sml lists, as they have a behaviour similar to that of
stacks*)
structure FunStack : STACK = struct
    type 'a Stack = 'a list
    exception EmptyStack
    exception StackTooSmall
    exception Error of string
    val create = [];
    fun push(item, stack) = item::stack;
    fun pop(stack) = tl(stack);
    fun top(stack) = hd(stack);
    fun empty(stack) = if(length(stack) = 0) then true else false;
    fun poptop(stack) = if(length(stack) = 0) then NONE else (SOME(hd(stack), tl(stack)));
    fun nth(stack, n) = if(n>length(stack)) then raise StackTooSmall else if(n=0) then hd(stack) else nth(tl(stack), n-1);
    fun drop(stack, n) = if(n>length(stack)) then raise StackTooSmall else if(n=0) then stack else drop(tl(stack), n-1);
    fun depth(stack) = List.length(stack);
    fun app f stack = List.app f stack;
    fun map f stack = List.map f stack;
    fun mapPartial f stack = List.mapPartial f stack;
    fun find f stack = List.find f stack;
    fun filter f stack = List.filter f stack;
    fun foldr f init stack = List.foldr f init stack;
    fun foldl f init stack = List.foldl f init stack;
    fun exists f stack = List.exists f stack;
    fun all f stack = List.all f stack;
    fun list2stack(list) = list;
    fun stack2list(stack) = stack;
    fun toString f stack = foldr op ^ "" (map f stack);
end; 

(*Exception to be used in the hashtable*)
exception VariableNotFound;

(*Function to check if two strings are equal - to be used in the hash table*)
fun equalString(string1 : string, string2) =
if string1 = string2 then true else false;

(*Global hash table to store the indices of the various variables in a program in their memory array*)
val newHash : (string, int) HashTable.hash_table = HashTable.mkTable (HashString.hashString, equalString) (101, VariableNotFound);

(*Function to extract the declaration list from an AST*)
fun declrListFromAST(PROG(stringname, BLK(declrlist, cmds))) = declrlist;

(*Function to extract the list of variables from a single declaration*)
fun listVar(DEC(varlist, typ)) = varlist;

(*Function to add the variables in a variable list to the hash table, returns zero because return type doesn't matter*)
fun addList(varlist) = 
if(varlist=[]) then 0 else
let val x = HashTable.insert newHash (hd(varlist), (HashTable.numItems newHash)) in
addList(tl(varlist))
end;

(*Function to add the variables in a single declaration to the hash table*)
fun addDecVar(declr) = addList(listVar(declr));

(*Function to add the variables in a declaration list to the hash table*)
fun addDecList(declist) = 
if(declist = []) then 0 else
let val x = addDecVar(hd(declist)) in
addDecList(tl(declist))
end;

(*Function to add the variables in a program to the hash table*)
fun addVarProg(filename) = addDecList(declrListFromAST(Wast.compile(filename)));

(*Function to extract the command sequence from an AST*)
fun astToCmd(PROG(stringname, BLK(declrlist, cmds))) = cmds; 

(*Function to convert an expression of the datatype Exp to a list of strings in postorder form*)
(*The "@" is put before the constructors to differentiate them from possible variables that may have the same name, as variable
names cannot start with "@"*)
fun exp2List(PLUS(exp1, exp2)) = (exp2List(exp1) @ exp2List(exp2)) @ ["@PLUS"]
    | exp2List(MINUS(exp1, exp2)) = (exp2List(exp1) @ exp2List(exp2)) @ ["@MINUS"]
    | exp2List(TIMES(exp1, exp2)) = (exp2List(exp1) @ exp2List(exp2)) @ ["@TIMES"]
    | exp2List(DIV(exp1, exp2)) = (exp2List(exp1) @ exp2List(exp2)) @ ["@DIV"]
    | exp2List(MOD(exp1, exp2)) = (exp2List(exp1) @ exp2List(exp2)) @ ["@MOD"]
    | exp2List(NEGATIVE(exp)) = exp2List(exp) @ ["@NEGATIVE"]
    | exp2List(LOGICALNOT(exp)) = exp2List(exp) @ ["@LOGICALNOT"]
    | exp2List(OR(exp1, exp2)) = (exp2List(exp1) @ exp2List(exp2)) @ ["@OR"]
    | exp2List(AND(exp1, exp2)) = (exp2List(exp1) @ exp2List(exp2)) @ ["@AND"]
    | exp2List(LT(exp1, exp2)) = (exp2List(exp1) @ exp2List(exp2)) @ ["@LT"]
    | exp2List(LEQ(exp1, exp2)) = (exp2List(exp1) @ exp2List(exp2)) @ ["@LEQ"]
    | exp2List(EQ(exp1, exp2)) = (exp2List(exp1) @ exp2List(exp2)) @ ["@EQ"]
    | exp2List(GT(exp1, exp2)) = (exp2List(exp1) @ exp2List(exp2)) @ ["@GT"]
    | exp2List(GEQ(exp1, exp2)) = (exp2List(exp1) @ exp2List(exp2)) @ ["@GEQ"]
    | exp2List(NEQ(exp1, exp2)) = (exp2List(exp1) @ exp2List(exp2)) @ ["@NEQ"]
    | exp2List(VAR(variable)) = [variable]
    | exp2List(NUMBER(numeral)) = [numeral]
    | exp2List(FF) = ["@FF"]
    | exp2List(TT) = ["@TT"];

(*Functions to convert commands and command sequences to lists of strings in postorder form, the "$" is a special symbol denoting
the start of a new command (will help in if then else statements and while loops), one can just assume that the arity of each 
constructor is increased by one and the first argument of each constructor is now "$", including the empty constructor*)
fun cmd2List(SET(variable, exp)) = "$" :: (variable :: (exp2List(exp) @ ["@SET"]))
    | cmd2List(READ(variable)) = "$" :: (variable :: ["@READ"])
    | cmd2List(WRITE(exp)) = "$" :: (exp2List(exp) @ ["@WRITE"])
    | cmd2List(ITE(exp, cmds1, cmds2)) = "$" :: ((exp2List(exp) @ cmdSeq2List(cmds1)) @ cmdSeq2List(cmds2)) @ ["@ITE"]
    | cmd2List(WH(exp, cmds)) = "$" :: (exp2List(exp) @ cmdSeq2List(cmds)) @ ["@WH"]

and cmdSeq2List(empty) = [""]
    | cmdSeq2List(SEQ(cmd, cmds))  = (cmd2List(cmd) @ cmdSeq2List(cmds)) @ ["@SEQ"];

(*Function to give the postorder list of an AST*)
fun postOrder(ast) = FunStack.list2stack(cmdSeq2List(astToCmd(ast)));

(*Function to give the postfix of a file containing a program in the WHILE language*)
fun postfixOut(filename) = postOrder(Wast.compile(filename));

(*Strings on which a command which has been converted to a list of strings can end*)
fun cmdEnd(s) = if(s="@SET" orelse s="@READ" orelse s="@WRITE" orelse s="@ITE" orelse s="@WH") then true else false;

(*If a list starts with a "$" (denotes a command), then this function gives the index where the command ends, kind of like parentheses
matching*)
fun indexCmd(l) = if(l=[] orelse hd(l) <> "$") then (~1) else
let fun iter_ind(l,c,n) = 
    if(c=0) then n 
    else if(hd(l)="$") then iter_ind(tl(l),c+1,n+1) 
    else if(cmdEnd(hd(l))) then iter_ind(tl(l),c-1,n+1)
    else iter_ind(tl(l),c,n+1)
in
iter_ind(tl(l),1,0)
end;

(*If a list has a command as a prefix list, then this function gives that prefix command list*)
fun extractCmd(l) = List.take(l,indexCmd(l)+1);

(*If a list has a command as a prefix list, then this function gives the rest of the list*)
fun removeCmd(l) = List.drop(l,indexCmd(l)+1);

(*Function to exrtract an entire command sequence from a list which has one as a prefix*)
fun extractCmds(l) = if(hd(l)="" andalso hd(tl(l)) = "@SEQ") then ["","@SEQ"] else (extractCmd(l) @ extractCmds(removeCmd(l)));

(*Function to remove the command sequence from a list if it is present as a prefix*)
fun removeCmds(l) = if(hd(l)="" andalso hd(tl(l)) = "@SEQ") then tl(tl(l)) else (removeCmds(removeCmd(l)));
(*Function to check if a string is a variable of the hash table*)
fun isVar(s) = if(HashTable.inDomain newHash s) then true else false;

(*Function to check if a string is a numeral, assuming that only numerals and variables are the strings allowed*)
fun isLiteral(s) = if((String.isPrefix "+" s) orelse (String.isPrefix "~" s) orelse (String.isPrefix "0" s) orelse (String.isPrefix "1" s) 
orelse (String.isPrefix "2" s) orelse (String.isPrefix "3" s) orelse (String.isPrefix "4" s) orelse (String.isPrefix "5" s)
orelse (String.isPrefix "6" s) orelse (String.isPrefix "7" s) orelse (String.isPrefix "8" s) orelse (String.isPrefix "9" s)) then true else false;

(*Function to create the intial configuration of the VMC machine given a program*)
fun createVMC(filename) =
let val x = HashTable.clear newHash; val v = FunStack.list2stack(tl([""])); val y = addVarProg(filename); val m = Array.array (HashTable.numItems newHash, 0); val c = postfixOut(filename) in
(v,m,c)
end;

(*Function to find the value of a string given the memory array - the string is either an integer or a variable*)
fun valueOf(s,m) = if(isVar(s)) then (Array.sub(m,HashTable.lookup newHash s)) else valOf(Int.fromString(s));

(*Function to find where a string "s" comes in a list of strings "l"*)
fun findInList(s,l) = let fun iter(s,l,n) = if(l=[]) then ~1 else if(hd(l) = s) then n else iter(s,tl(l),n+1) in iter(s,l,0) end;

(*Function to remove all the elements of a list till the first "$"*)
fun removeTillFirst(l) = let val x = findInList("$",l) in List.drop(l, x) end;

(*Function to take all the elements of a list till the first "$"*)
fun takeTillFirst(l) = let val x = findInList("$",l) in List.take(l, x) end;

(*Function to check if a while command is empty*)
fun isEmptyWh(l) = let val c = extractCmd(l) in if(findInList("$",tl(c))= ~1) then true else false end;

(*Function to take out the expression if a while command is empty*)
fun expEmptyWh(l) = let val x = findInList("",l) in tl(List.take(l,x)) end;

(*Function to take the boolean expression out of an ITE statement*)
fun expITE(l) = let val x = findInList("$",l) in (List.take(l,x)) end;

(*Function to remove empty commands and the "SEQ" nodes which are not needed for the computations*)
fun removeUseless(l) = if(l=[]) then [] else if hd(l) = "" then removeUseless(tl(l)) else if hd(l) = "@SEQ" then removeUseless(tl(l))  else l;

(*Function to evaluate the contents of a list which is just the postorder form of an expression, this is similar to the rules
function defined just below it*)
fun evaluateList(v: string FunStack.Stack, m: int array, c: string FunStack.Stack) =
    if(c = FunStack.create) then valOf(Int.fromString(FunStack.top(v))) else

    (*Transferring variables or literals to value stack*)
    if(FunStack.top(c) = "@TT") then evaluateList(FunStack.push("1",v), m, FunStack.pop(c)) else
    if(FunStack.top(c) = "@FF") then evaluateList(FunStack.push("0",v), m, FunStack.pop(c)) else
    if(isLiteral(FunStack.top(c))) then evaluateList(FunStack.push(FunStack.top(c),v), m, FunStack.pop(c)) else
    if(isVar(FunStack.top(c))) then evaluateList(FunStack.push(FunStack.top(c),v), m, FunStack.pop(c)) else

    (*Operators - binary and unary both*)
    if(FunStack.top(c) = "@PLUS") then let val (a,v1) = valOf(FunStack.poptop(v)); val (b,v2) = valOf(FunStack.poptop(v1)) 
    in evaluateList(FunStack.push(Int.toString(valueOf(b,m)+valueOf(a,m)),v2),m,FunStack.pop(c)) end else

    if(FunStack.top(c) = "@MINUS") then let val (a,v1) = valOf(FunStack.poptop(v)); val (b,v2) = valOf(FunStack.poptop(v1)) 
    in evaluateList(FunStack.push(Int.toString(valueOf(b,m)-valueOf(a,m)),v2),m,FunStack.pop(c)) end else

    if(FunStack.top(c) = "@TIMES") then let val (a,v1) = valOf(FunStack.poptop(v)); val (b,v2) = valOf(FunStack.poptop(v1)) 
    in evaluateList(FunStack.push(Int.toString(valueOf(b,m)*valueOf(a,m)),v2),m,FunStack.pop(c)) end else

    if(FunStack.top(c) = "@DIV") then let val (a,v1) = valOf(FunStack.poptop(v)); val (b,v2) = valOf(FunStack.poptop(v1)) 
    in evaluateList(FunStack.push(Int.toString(valueOf(b,m) div valueOf(a,m)),v2),m,FunStack.pop(c)) end else

    if(FunStack.top(c) = "@MOD") then let val (a,v1) = valOf(FunStack.poptop(v)); val (b,v2) = valOf(FunStack.poptop(v1)) 
    in evaluateList(FunStack.push(Int.toString(valueOf(b,m) mod valueOf(a,m)),v2),m,FunStack.pop(c)) end else

    if(FunStack.top(c) = "@OR") then let val (a,v1) = valOf(FunStack.poptop(v)); val (b,v2) = valOf(FunStack.poptop(v1)) 
    in if(a="0" andalso b="0") then evaluateList(FunStack.push("0",v2),m,FunStack.pop(c)) else evaluateList(FunStack.push("1",v2),m,FunStack.pop(c)) end else

    if(FunStack.top(c) = "@AND") then let val (a,v1) = valOf(FunStack.poptop(v)); val (b,v2) = valOf(FunStack.poptop(v1)) 
    in if(a="1" andalso b="1") then evaluateList(FunStack.push("1",v2),m,FunStack.pop(c)) else evaluateList(FunStack.push("0",v2),m,FunStack.pop(c)) end else

    if(FunStack.top(c) = "@LT") then let val (a,v1) = valOf(FunStack.poptop(v)); val (b,v2) = valOf(FunStack.poptop(v1)) 
    in if(valueOf(b,m)<valueOf(a,m)) then evaluateList(FunStack.push("1",v2),m,FunStack.pop(c)) else evaluateList(FunStack.push("0",v2),m,FunStack.pop(c)) end else

    if(FunStack.top(c) = "@LEQ") then let val (a,v1) = valOf(FunStack.poptop(v)); val (b,v2) = valOf(FunStack.poptop(v1)) 
    in if(valueOf(b,m)<=valueOf(a,m)) then evaluateList(FunStack.push("1",v2),m,FunStack.pop(c)) else evaluateList(FunStack.push("0",v2),m,FunStack.pop(c)) end else

    if(FunStack.top(c) = "@EQ") then let val (a,v1) = valOf(FunStack.poptop(v)); val (b,v2) = valOf(FunStack.poptop(v1)) 
    in if(valueOf(b,m)=valueOf(a,m)) then evaluateList(FunStack.push("1",v2),m,FunStack.pop(c)) else evaluateList(FunStack.push("0",v2),m,FunStack.pop(c)) end else

    if(FunStack.top(c) = "@GT") then let val (a,v1) = valOf(FunStack.poptop(v)); val (b,v2) = valOf(FunStack.poptop(v1)) 
    in if(valueOf(b,m)>valueOf(a,m)) then evaluateList(FunStack.push("1",v2),m,FunStack.pop(c)) else evaluateList(FunStack.push("0",v2),m,FunStack.pop(c)) end else

    if(FunStack.top(c) = "@GEQ") then let val (a,v1) = valOf(FunStack.poptop(v)); val (b,v2) = valOf(FunStack.poptop(v1)) 
    in if(valueOf(b,m)>=valueOf(a,m)) then evaluateList(FunStack.push("1",v2),m,FunStack.pop(c)) else evaluateList(FunStack.push("0",v2),m,FunStack.pop(c)) end else

    if(FunStack.top(c) = "@NEQ") then let val (a,v1) = valOf(FunStack.poptop(v)); val (b,v2) = valOf(FunStack.poptop(v1)) 
    in if(valueOf(b,m)<>valueOf(a,m)) then evaluateList(FunStack.push("1",v2),m,FunStack.pop(c)) else evaluateList(FunStack.push("0",v2),m,FunStack.pop(c)) end else

    if(FunStack.top(c) = "@NEGATIVE") then let val (a,v1) = valOf(FunStack.poptop(v))
    in evaluateList(FunStack.push(Int.toString(0-valueOf(a,m)), v1), m, FunStack.pop(c)) end else

    if(FunStack.top(c) = "@LOGICALNOT") then let val (a,v1) = valOf(FunStack.poptop(v))
    in evaluateList(FunStack.push(Int.toString((1+valueOf(a,m)) mod 2), v1), m, FunStack.pop(c)) end else
    (~1);

(*Function for carrying out the transition rules, design choices were made in this function*)
fun rulesOut(v: string FunStack.Stack, m: int array, c: string FunStack.Stack) =
    (*Base case and rule C.; covered here*)
    if(c = FunStack.create) then (v, m, c) else
    if(FunStack.top(c)="@SEQ") then (v, m, FunStack.pop(c)) else

    (*Transferring variables or literals to value stack - rules E.m, E.x and the next four rules E.00, E.11, E.10 and E.01 are
    covered in this*)
    if(FunStack.top(c) = "@TT") then (FunStack.push("1",v), m, FunStack.pop(c)) else
    if(FunStack.top(c) = "@FF") then (FunStack.push("0",v), m, FunStack.pop(c)) else
    if(isLiteral(FunStack.top(c))) then (FunStack.push(FunStack.top(c),v), m, FunStack.pop(c)) else
    if(isVar(FunStack.top(c))) then (FunStack.push(FunStack.top(c),v), m, FunStack.pop(c)) else

    (*Operators - binary and unary both*)
    (*The rule E. is covered here, along with accounting for unary operators which aren't mentioned in the pdf*)
    if(FunStack.top(c) = "@PLUS") then let val (a,v1) = valOf(FunStack.poptop(v)); val (b,v2) = valOf(FunStack.poptop(v1)) 
    in (FunStack.push(Int.toString(valueOf(b,m)+valueOf(a,m)),v2),m,FunStack.pop(c)) end else

    if(FunStack.top(c) = "@MINUS") then let val (a,v1) = valOf(FunStack.poptop(v)); val (b,v2) = valOf(FunStack.poptop(v1)) 
    in (FunStack.push(Int.toString(valueOf(b,m)-valueOf(a,m)),v2),m,FunStack.pop(c)) end else

    if(FunStack.top(c) = "@TIMES") then let val (a,v1) = valOf(FunStack.poptop(v)); val (b,v2) = valOf(FunStack.poptop(v1)) 
    in (FunStack.push(Int.toString(valueOf(b,m)*valueOf(a,m)),v2),m,FunStack.pop(c)) end else

    if(FunStack.top(c) = "@DIV") then let val (a,v1) = valOf(FunStack.poptop(v)); val (b,v2) = valOf(FunStack.poptop(v1)) 
    in (FunStack.push(Int.toString(valueOf(b,m) div valueOf(a,m)),v2),m,FunStack.pop(c)) end else

    if(FunStack.top(c) = "@MOD") then let val (a,v1) = valOf(FunStack.poptop(v)); val (b,v2) = valOf(FunStack.poptop(v1)) 
    in (FunStack.push(Int.toString(valueOf(b,m) mod valueOf(a,m)),v2),m,FunStack.pop(c)) end else

    if(FunStack.top(c) = "@OR") then let val (a,v1) = valOf(FunStack.poptop(v)); val (b,v2) = valOf(FunStack.poptop(v1)) 
    in if(a="0" andalso b="0") then (FunStack.push("0",v2),m,FunStack.pop(c)) else (FunStack.push("1",v2),m,FunStack.pop(c)) end else

    if(FunStack.top(c) = "@AND") then let val (a,v1) = valOf(FunStack.poptop(v)); val (b,v2) = valOf(FunStack.poptop(v1)) 
    in if(a="1" andalso b="1") then (FunStack.push("1",v2),m,FunStack.pop(c)) else (FunStack.push("0",v2),m,FunStack.pop(c)) end else

    if(FunStack.top(c) = "@LT") then let val (a,v1) = valOf(FunStack.poptop(v)); val (b,v2) = valOf(FunStack.poptop(v1)) 
    in if(valueOf(b,m)<valueOf(a,m)) then (FunStack.push("1",v2),m,FunStack.pop(c)) else (FunStack.push("0",v2),m,FunStack.pop(c)) end else

    if(FunStack.top(c) = "@LEQ") then let val (a,v1) = valOf(FunStack.poptop(v)); val (b,v2) = valOf(FunStack.poptop(v1)) 
    in if(valueOf(b,m)<=valueOf(a,m)) then (FunStack.push("1",v2),m,FunStack.pop(c)) else (FunStack.push("0",v2),m,FunStack.pop(c)) end else

    if(FunStack.top(c) = "@EQ") then let val (a,v1) = valOf(FunStack.poptop(v)); val (b,v2) = valOf(FunStack.poptop(v1)) 
    in if(valueOf(b,m)=valueOf(a,m)) then (FunStack.push("1",v2),m,FunStack.pop(c)) else (FunStack.push("0",v2),m,FunStack.pop(c)) end else

    if(FunStack.top(c) = "@GT") then let val (a,v1) = valOf(FunStack.poptop(v)); val (b,v2) = valOf(FunStack.poptop(v1)) 
    in if(valueOf(b,m)>valueOf(a,m)) then (FunStack.push("1",v2),m,FunStack.pop(c)) else (FunStack.push("0",v2),m,FunStack.pop(c)) end else

    if(FunStack.top(c) = "@GEQ") then let val (a,v1) = valOf(FunStack.poptop(v)); val (b,v2) = valOf(FunStack.poptop(v1)) 
    in if(valueOf(b,m)>=valueOf(a,m)) then (FunStack.push("1",v2),m,FunStack.pop(c)) else (FunStack.push("0",v2),m,FunStack.pop(c)) end else

    if(FunStack.top(c) = "@NEQ") then let val (a,v1) = valOf(FunStack.poptop(v)); val (b,v2) = valOf(FunStack.poptop(v1)) 
    in if(valueOf(b,m)<>valueOf(a,m)) then (FunStack.push("1",v2),m,FunStack.pop(c)) else (FunStack.push("0",v2),m,FunStack.pop(c)) end else

    if(FunStack.top(c) = "@NEGATIVE") then let val (a,v1) = valOf(FunStack.poptop(v))
    in (FunStack.push(Int.toString(0-valueOf(a,m)), v1), m, FunStack.pop(c)) end else

    if(FunStack.top(c) = "@LOGICALNOT") then let val (a,v1) = valOf(FunStack.poptop(v))
    in (FunStack.push(Int.toString((1+valueOf(a,m)) mod 2), v1), m, FunStack.pop(c)) end else

    (*Commands*)
    (*Rules C.:=0 and C.:=1 are covered here*)
    if(FunStack.top(c) = "$" andalso FunStack.nth(c,indexCmd(c))="@SET") then (v,m,FunStack.pop(c)) else

    if(FunStack.top(c) = "@SET") then let val (a,v1) = valOf(FunStack.poptop(v)); val (b, v2) = valOf(FunStack.poptop(v1));
    val cc = Array.update(m, (HashTable.lookup newHash b), valueOf(a,m)) in (v2,m,FunStack.pop(c)) end else

    (*Rules for reading are applied here, these aren't explicitly mentioned in the pdf*)
    if(FunStack.top(c) = "$" andalso FunStack.nth(c,indexCmd(c))="@READ") then (v,m,FunStack.pop(c)) else

    if(FunStack.top(c) = "@READ") then let val x = print "Input: " ;val y = Int.fromString(valOf(TextIO.inputLine(TextIO.stdIn)));
    val (z,v1) = valOf(FunStack.poptop(v)); val w = Array.update(m, (HashTable.lookup newHash z), valOf(y)) in
    (v1,m,FunStack.pop(c)) end else

    (*Rules for writing are applied here, these also aren't explicitly mentioned in the pdf*)
    if(FunStack.top(c) = "$" andalso FunStack.nth(c,indexCmd(c))="@WRITE") then (v,m,FunStack.pop(c)) else

    if(FunStack.top(c) = "@WRITE") then let val (x,v1) = valOf(FunStack.poptop(v)); val y = print(Int.toString(valueOf(x,m))^"\n") in
    (v1, m, FunStack.pop(c)) end else
    
    (*Rules C.ite? C.ite0 and C.ite1 are covered here*)
    (*These subcases are made when ITE is the command based on whether any of the expressions is empty or not (my implementation 
    treats empty command sequences a little differently from non-empty ones that's why) *)
    if(FunStack.top(c) = "$" andalso FunStack.nth(c,indexCmd(c))="@ITE") then let val w = extractCmd(c); val x = List.rev(w) in 
    if(hd(tl(x)) = "" andalso hd(tl(tl(x)))="") then (v,m,removeCmd(c)) else
    let val d = removeTillFirst(tl(c)) in
    if(hd(rev(takeTillFirst(tl(w))))="") then if(evaluateList([],m,expEmptyWh(w))=1) then (v,m,removeCmd(c)) else
    (v,m,removeTillFirst(tl(c))) else
    if(hd(tl(rev(w)))="") then if(evaluateList([],m,expITE(tl(w)))=1) then (v,m,removeTillFirst(tl(c))) else (v,m,removeCmd(c)) else
    if(evaluateList([],m,takeTillFirst(tl(c)))=1) then
    (v,m,(extractCmds(d) @ removeCmds(removeUseless(removeCmds(d))))) else
    (v,m,tl(removeCmds(d))) end end else
    
    (*Rules C.wh? C.wh0 and C.wh1 are covered here*)
    (*Similarly, the subcases are made for the WH commands handling empty command sequences*)
    if(FunStack.top(c) = "$" andalso FunStack.nth(c,indexCmd(c))="@WH") then if(isEmptyWh(c)=true) then if(evaluateList([],m,expEmptyWh(c))=1) then (v,m,c)
    else (v,m,removeCmd(c)) else
    let val x = takeTillFirst(FunStack.stack2list(FunStack.pop(c)));
    val y = evaluateList(FunStack.create,m,FunStack.list2stack(x)); val z = removeTillFirst(FunStack.stack2list(FunStack.pop(c)));
    val a = extractCmds(z); val b = removeCmds(z); val cc = removeUseless(b) in
    if(y=0) then (v,m,FunStack.list2stack(cc)) else (v,m,FunStack.list2stack(a @ (FunStack.stack2list(c)))) end else

    (*If any of these happen on the top of the control stack, then simply pop them*)
    if(FunStack.top(c) ="@ITE") then (v,m,FunStack.pop(c)) else
    if(FunStack.top(c) ="@WH") then (v,m,FunStack.pop(c)) else
    (*Rule C.{} is covered here*)
    if(FunStack.top(c) ="") then (v,m,FunStack.pop(c))

    else (v,m,c);

(*Function to recursively apply rules to an initial configuration and find out the final configuration if it exists*)
fun evaluateTriple(v,m,c) = if (c=FunStack.create) then (v,m,c) else evaluateTriple(rulesOut(v,m,c));

(*Function to evaluate the final configuration of a WHILE program in a file*)
fun evaluateOut(filename) = evaluateTriple(createVMC(filename));

(*Function to conert an integer list to a list of strings*)
fun intlisttostring(l) = if(l=[]) then [] else (Int.toString(hd(l)))::(intlisttostring(tl(l)));

(*Signature VMC*)
signature VMC =
sig
    type VMC = (string FunStack.Stack)*(int array)*(string FunStack.Stack)
    val create : string -> VMC
    val rules : VMC -> VMC
    val toString : VMC -> (string list)*(string list)*(string list)
    val postfix : Ast -> string FunStack.Stack
    val execute : string -> VMC
end

structure Vmc : VMC = struct
    type VMC = (string FunStack.Stack)*(int array)*(string FunStack.Stack)

    (*Creates a VMC machine for a program in a file*)
    fun create(filename) = createVMC(filename);

    (*Applies the rules on a vmc machine to give updated vmc machine*)
    fun rules(vmc) = rulesOut(vmc);

    (*Converts a vmc machine to a string*)
    fun toString(v,m,c) = (FunStack.stack2list(v),intlisttostring(Array.foldr(op ::)[] m), FunStack.stack2list(c));

    (*Makes the postfix of an AST*)
    fun postfix(tre) = postOrder(tre);

    (*Executes a program written in the WHILE language and gives the final configuration of the VMC*)
    fun execute(filename) = evaluateOut(filename);
end