structure WastLrVals = WastLrValsFun(
            structure Token = LrParser.Token);
structure WastLex    = WastLexFun(
            structure Tokens = WastLrVals.Tokens);
structure WastParser = JoinWithArg(
            structure ParserData = WastLrVals.ParserData
            structure Lex=WastLex
            structure LrParser=LrParser);