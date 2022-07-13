signature DATATYPES =
sig type Variable = string
    type Typ = string
    type Numeral = string
    datatype Ast = PROG of string * Block
    and Block = BLK of Declr list * CmdSeq
    and Declr = DEC  of Variable list * Typ 
    and CmdSeq = empty | SEQ of Cmd * CmdSeq
    and Cmd = SET of Variable * Exp
            | READ of Variable
            | WRITE of Exp
            | ITE of Exp * CmdSeq * CmdSeq
            | WH of Exp * CmdSeq
    and Exp = PLUS of Exp * Exp
            | MINUS of Exp * Exp
            | TIMES of Exp * Exp
            | DIV of Exp * Exp
            | MOD of Exp * Exp
            | NEGATIVE of Exp
            | LOGICALNOT of Exp
            | OR of Exp * Exp
            | AND of Exp * Exp
            | LT of Exp * Exp
            | LEQ of Exp * Exp
            | EQ of Exp * Exp
            | GT of Exp * Exp
            | GEQ of Exp * Exp
            | NEQ of Exp * Exp
            | VAR of Variable
            | NUMBER of Numeral
            | FF
            | TT
end;

structure DataTypes : DATATYPES =
struct
    type Variable = string
    type Typ = string
    type Numeral = string
    datatype Ast = PROG of string * Block
    and Block = BLK of Declr list * CmdSeq
    and Declr = DEC  of Variable list * Typ 
    and CmdSeq = empty | SEQ of Cmd * CmdSeq
    and Cmd = SET of Variable * Exp
            | READ of Variable
            | WRITE of Exp
            | ITE of Exp * CmdSeq * CmdSeq
            | WH of Exp * CmdSeq
    and Exp = PLUS of Exp * Exp
            | MINUS of Exp * Exp
            | TIMES of Exp * Exp
            | DIV of Exp * Exp
            | MOD of Exp * Exp
            | NEGATIVE of Exp
            | LOGICALNOT of Exp
            | OR of Exp * Exp
            | AND of Exp * Exp
            | LT of Exp * Exp
            | LEQ of Exp * Exp
            | EQ of Exp * Exp
            | GT of Exp * Exp
            | GEQ of Exp * Exp
            | NEQ of Exp * Exp
            | VAR of Variable
            | NUMBER of Numeral
            | FF
            | TT
end;