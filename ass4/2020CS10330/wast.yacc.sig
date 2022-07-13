signature Wast_TOKENS =
sig
type ('a,'b) token
type svalue
val EOF:  'a * 'a -> (svalue,'a) token
val BADCH:  'a * 'a -> (svalue,'a) token
val FF:  'a * 'a -> (svalue,'a) token
val TT:  'a * 'a -> (svalue,'a) token
val ENDWHH:  'a * 'a -> (svalue,'a) token
val DOO:  'a * 'a -> (svalue,'a) token
val WHILEE:  'a * 'a -> (svalue,'a) token
val ENDIFF:  'a * 'a -> (svalue,'a) token
val ELSEE:  'a * 'a -> (svalue,'a) token
val THENN:  'a * 'a -> (svalue,'a) token
val IFF:  'a * 'a -> (svalue,'a) token
val WRITEE:  'a * 'a -> (svalue,'a) token
val READD:  'a * 'a -> (svalue,'a) token
val BOOLL:  'a * 'a -> (svalue,'a) token
val INTT:  'a * 'a -> (svalue,'a) token
val VARR:  'a * 'a -> (svalue,'a) token
val PROGRAMM:  'a * 'a -> (svalue,'a) token
val NUM: (string) *  'a * 'a -> (svalue,'a) token
val NEQQ:  'a * 'a -> (svalue,'a) token
val GEQQ:  'a * 'a -> (svalue,'a) token
val GTT:  'a * 'a -> (svalue,'a) token
val EQQ:  'a * 'a -> (svalue,'a) token
val LEQQ:  'a * 'a -> (svalue,'a) token
val LTT:  'a * 'a -> (svalue,'a) token
val ANDD:  'a * 'a -> (svalue,'a) token
val ORR:  'a * 'a -> (svalue,'a) token
val NOTT:  'a * 'a -> (svalue,'a) token
val RPAR:  'a * 'a -> (svalue,'a) token
val LPAR:  'a * 'a -> (svalue,'a) token
val MODD:  'a * 'a -> (svalue,'a) token
val DIVV:  'a * 'a -> (svalue,'a) token
val MUL:  'a * 'a -> (svalue,'a) token
val MINUSS:  'a * 'a -> (svalue,'a) token
val PLUSS:  'a * 'a -> (svalue,'a) token
val NEG:  'a * 'a -> (svalue,'a) token
val ASSIGN:  'a * 'a -> (svalue,'a) token
val COMMA:  'a * 'a -> (svalue,'a) token
val SCOLON:  'a * 'a -> (svalue,'a) token
val COLON:  'a * 'a -> (svalue,'a) token
val RBRACE:  'a * 'a -> (svalue,'a) token
val LBRACE:  'a * 'a -> (svalue,'a) token
val CONS:  'a * 'a -> (svalue,'a) token
val IDE: (string) *  'a * 'a -> (svalue,'a) token
end
signature Wast_LRVALS=
sig
structure Tokens : Wast_TOKENS
structure ParserData:PARSER_DATA
sharing type ParserData.Token.token = Tokens.token
sharing type ParserData.svalue = Tokens.svalue
end
