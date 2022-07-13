signature DATATYPES =
sig type Variable = string
    type Typ = string
    datatype Ast = PROG of string * Block
    and Block = BLK of Declr list * CmdSeq
    and Declr = DEC  of Variable list * Typ
    and CmdSeq = empty | SEQ of Cmd * CmdSeq
    and Cmd = SETINT of Variable * Intexp
            | SETBOOL of Variable * Boolexp
            | READINT of Variable
            | READBOOL of Variable
            | WRITEINT of Intexp
            | WRITEBOOL of Boolexp
            | ITE of Boolexp * CmdSeq * CmdSeq
            | WH of Boolexp * CmdSeq
    and Intexp = PLUS of Intterm * Intexp
            | MINUS of Intterm * Intexp
            | ITERM of Intterm
    and Intterm = TIMES of Intfac * Intterm
            | DIV of Intfac * Intterm
            | MOD of Intfac * Intterm
            | IFAC of Intfac
    and Intfac = IVAR of Variable
               | NUMB of string
               | NEGVE of Intfac
               | IEXP of Intexp
    and Boolexp = OR of Boolterm * Boolexp
            | BTERM of Boolterm
    and Boolterm = AND of Boolfac * Boolterm
                 | BFAC of Boolfac
    and Boolfac = TT
                | FF
                | BVAR of Variable
                | COMP of Comp
                | BEXP of Boolexp
                | LOGNOT of Boolfac
    and Comp = INEQ of Intexp * Intexp 
            | ILT of Intexp * Intexp
            | ILEQ of Intexp * Intexp
            | IGT of Intexp * Intexp
            | IGEQ of Intexp * Intexp
            | IEQ of Intexp * Intexp
            | BNEQ of Boolexp * Boolexp
            | BLT of Boolexp * Boolexp
            | BLEQ of Boolexp * Boolexp
            | BGT of Boolexp * Boolexp
            | BGEQ of Boolexp * Boolexp
            | BEQ of Boolexp * Boolexp
end;

structure DataTypes : DATATYPES =
struct
    type Variable = string
    type Typ = string
    datatype Ast = PROG of string * Block
    and Block = BLK of Declr list * CmdSeq
    and Declr = DEC  of Variable list * Typ
    and CmdSeq = empty | SEQ of Cmd * CmdSeq
    and Cmd = SETINT of Variable * Intexp
            | SETBOOL of Variable * Boolexp
            | READINT of Variable
            | READBOOL of Variable
            | WRITEINT of Intexp
            | WRITEBOOL of Boolexp
            | ITE of Boolexp * CmdSeq * CmdSeq
            | WH of Boolexp * CmdSeq
    and Intexp = PLUS of Intterm * Intexp
            | MINUS of Intterm * Intexp
            | ITERM of Intterm
    and Intterm = TIMES of Intfac * Intterm
            | DIV of Intfac * Intterm
            | MOD of Intfac * Intterm
            | IFAC of Intfac
    and Intfac = IVAR of Variable
               | NUMB of string
               | NEGVE of Intfac
               | IEXP of Intexp
    and Boolexp = OR of Boolterm * Boolexp
            | BTERM of Boolterm
    and Boolterm = AND of Boolfac * Boolterm
                 | BFAC of Boolfac
    and Boolfac = TT
                | FF
                | BVAR of Variable
                | COMP of Comp
                | BEXP of Boolexp
                | LOGNOT of Boolfac
    and Comp = INEQ of Intexp * Intexp 
            | ILT of Intexp * Intexp
            | ILEQ of Intexp * Intexp
            | IGT of Intexp * Intexp
            | IGEQ of Intexp * Intexp
            | IEQ of Intexp * Intexp
            | BNEQ of Boolexp * Boolexp
            | BLT of Boolexp * Boolexp
            | BLEQ of Boolexp * Boolexp
            | BGT of Boolexp * Boolexp
            | BGEQ of Boolexp * Boolexp
            | BEQ of Boolexp * Boolexp
end;