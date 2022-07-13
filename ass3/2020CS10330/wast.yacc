open DataTypes
%%
%name Wast
%term IDE of string | NUM of string | CONS | COLON | SCOLON 
    | COMMA | LBRACE | RBRACE | ASSIGN | NEG | ORR | ANDD 
    | LPAR | RPAR | NOTT | LTT | LEQQ | EQQ | GTT | GEQQ | NEQQ 
    | PLUSS | MINUSS | MUL | DIVV | MODD | BADCH | EOF | PROGRAMM | VARR | INTT | BOOLL | READD | WRITEE | IFF | THENN | ELSEE
    | ENDIFF | WHILEE | DOO | ENDWHH | TT | FF 
%nonterm begin of Ast |  block of Block 
    | declr of Declr  | variable of Variable | cmds of CmdSeq  
    | cmdd of Cmd | declrlist of Declr list | typ of Typ
    | comp of Comp | varlistcom of Variable list | intexp of Intexp | intterm of Intterm 
    | intfac of Intfac | boolexp of Boolexp | boolterm of Boolterm | boolfac of Boolfac | varlist of Variable list
%pos int
%eop EOF
%noshift EOF
%nonassoc SCOLON COMMA LBRACE RBRACE LPAR RPAR EQQ 
%left  MINUSS DIVV MODD
%right CONS
%nodefault
%verbose
%keyword PROGRAMM VARR INTT BOOLL READD WRITEE IFF THENN ELSEE ENDIFF WHILEE DOO ENDWHH TT FF
%arg(fileName) : string


%%
begin: PROGRAMM IDE CONS block                                                  (PROG(IDE,block))       
block : declrlist LBRACE cmds RBRACE                                            (BLK(declrlist, cmds))
declrlist : declr SCOLON declrlist                                              (declr::declrlist)
          |                                                                     ([])
declr : VARR varlist COLON typ                                                  (DEC(varlist,typ))
typ : INTT                                                                      ("int")
    | BOOLL                                                                     ("bool")
varlist : variable varlistcom                                                   (variable :: varlistcom)
varlistcom : COMMA variable varlistcom                                          (variable :: varlistcom)       
     |                                                                          ([])                          
cmds : cmdd SCOLON cmds                                                         (SEQ(cmdd,cmds))
     |                                                                          (empty)     
cmdd : variable ASSIGN intexp                                                   (SETINT(variable, intexp))
     | variable ASSIGN boolexp                                                  (SETBOOL(variable,boolexp))
     | READD variable                                                           (READINT(variable))
     | READD variable                                                           (READBOOL(variable))
     | WRITEE intexp                                                            (WRITEINT(intexp))
     | WRITEE boolexp                                                           (WRITEBOOL(boolexp))
     | IFF boolexp THENN LBRACE cmds RBRACE ELSEE LBRACE cmds RBRACE ENDIFF     (ITE(boolexp, cmds1, cmds2))
     | WHILEE boolexp DOO LBRACE cmds RBRACE ENDWHH                             (WH(boolexp,cmds))     
intexp : intterm PLUSS intexp                                                   (PLUS(intterm,intexp))
       | intterm MINUSS intexp                                                  (MINUS(intterm,intexp))
       | intterm                                                                (ITERM(intterm))
intterm : intfac MUL intterm                                                    (TIMES(intfac,intterm))
        | intfac DIVV intterm                                                   (DIV(intfac,intterm))
        | intfac MODD intterm                                                   (MOD(intfac,intterm))
        | intfac                                                                (IFAC(intfac))
intfac : variable                                                               (IVAR(variable))
       | PLUSS NUM                                                              (NUMB("+"^NUM))
       | NEG NUM                                                                (NUMB("~"^NUM))
       | NUM                                                                    (NUMB(NUM))
       | NEG intfac                                                             (NEGVE(intfac))
       | LPAR intexp RPAR                                                       (IEXP(intexp))
boolexp : boolterm ORR boolexp                                                  (OR(boolterm,boolexp))
        | boolterm                                                              (BTERM(boolterm))
boolterm : boolfac ANDD boolterm                                                (AND(boolfac,boolterm))
         | boolfac                                                              (BFAC(boolfac))
boolfac : TT                                                                    (TT)
        | FF                                                                    (FF)                         
        | variable                                                              (BVAR(variable))
        | comp                                                                  (COMP(comp))
        | LPAR boolexp RPAR                                                     (BEXP(boolexp))  
        | NOTT boolfac                                                          (LOGNOT(boolfac))
comp : intexp LTT intexp                                                        (ILT(intexp1,intexp2))
     | intexp LEQQ intexp                                                       (ILEQ(intexp1,intexp2))
     | intexp GTT intexp                                                        (IGT(intexp1,intexp2))
     | intexp GEQQ intexp                                                       (IGEQ(intexp1,intexp2))
     | intexp EQQ intexp                                                        (IEQ(intexp1,intexp2))
     | intexp NEQQ intexp                                                       (INEQ(intexp1,intexp2)) 
     | boolexp LTT boolexp                                                      (BLT(boolexp1,boolexp2))
     | boolexp LEQQ boolexp                                                     (BLEQ(boolexp1,boolexp2))
     | boolexp GTT boolexp                                                      (BGT(boolexp1,boolexp2))
     | boolexp GEQQ boolexp                                                     (BGEQ(boolexp1,boolexp2))
     | boolexp EQQ boolexp                                                      (BEQ(boolexp1,boolexp2))
     | boolexp NEQQ boolexp                                                     (BNEQ(boolexp1,boolexp2))
variable : IDE                                                                  (IDE)