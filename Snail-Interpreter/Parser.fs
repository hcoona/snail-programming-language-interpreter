module Parser

open FParsec

let ws = spaces
let ws1 = spaces1

let skipStr_ws s = skipString s .>> ws
let skipStr_ws1 s = skipString s .>> ws1

let int_lit = pint32 .>> ws |>> AST.INT

let keywords = ["if"; "then"; "else"; "endif"; "print"]
let keywordsSet = new System.Collections.Generic.HashSet<string>(keywords)
let isKeyword str = keywordsSet.Contains(str)
let id : Parser<string, unit> =
    let identifierString = IdentifierOptions() |> identifier .>> ws
    let expectedIdentifier = expected "identifier"
    fun stream ->
        let state = stream.State
        let reply = identifierString stream
        if reply.Status <> Ok || not (isKeyword reply.Result) then reply
        else // result is keyword, so backtrack to before the string
            stream.BacktrackTo(state)
            Reply(Error, expectedIdentifier)

let opp = new OperatorPrecedenceParser<AST.expr, unit, unit>()

let expr = opp.ExpressionParser

let term = int_lit <|> between (skipStr_ws "(") (skipStr_ws ")") expr <|> (id |>> AST.ID)

opp.TermParser <- term

type Assoc = FParsec.Associativity
opp.AddOperator(PrefixOperator("-", ws, 300, false, fun x -> AST.UniOpExpr (AST.Neg, x)))
opp.AddOperator(InfixOperator("+", ws, 100, Assoc.Left, fun x y -> AST.BinOpExpr (AST.Plus, x, y)))
opp.AddOperator(InfixOperator("-", ws, 100, Assoc.Left, fun x y -> AST.BinOpExpr (AST.Minus, x, y)))
opp.AddOperator(InfixOperator("*", ws, 200, Assoc.Left, fun x y -> AST.BinOpExpr (AST.Times, x, y)))
opp.AddOperator(InfixOperator("/", ws, 200, Assoc.Left, fun x y -> AST.BinOpExpr (AST.Division, x, y)))
opp.AddOperator(InfixOperator("<", ws, 50, Assoc.Left, fun x y -> AST.BinOpExpr (AST.LT, x, y)))
opp.AddOperator(InfixOperator("<=", ws, 50, Assoc.Left, fun x y -> AST.BinOpExpr (AST.LEQ, x, y)))
opp.AddOperator(InfixOperator(">", ws, 50, Assoc.Left, fun x y -> AST.BinOpExpr (AST.GT, x, y)))
opp.AddOperator(InfixOperator(">=", ws, 50, Assoc.Left, fun x y -> AST.BinOpExpr (AST.GEQ, x, y)))
opp.AddOperator(InfixOperator("==", ws, 50, Assoc.Left, fun x y -> AST.BinOpExpr (AST.EQ, x, y)))
opp.AddOperator(InfixOperator("!=", ws, 50, Assoc.Left, fun x y -> AST.BinOpExpr (AST.NEQ, x, y)))

let stmtEnd = skipStr_ws ";"

let stmt, stmtImp = createParserForwardedToRef()

let assign = id .>>. ((skipStr_ws "=") >>. expr) .>> stmtEnd |>> (AST.Assign >> AST.AssignStmt)

let print = 
    let printExpr = expr .>> stmtEnd |>> AST.PrintExpr
    let printNewline = skipStr_ws "newline" .>>? stmtEnd >>. preturn AST.PrintNewline
    let printString = between (pstring "\"") (pstring "\"") (manyChars (noneOf "\"\n")) .>> stmtEnd |>> AST.PrintString
    skipStr_ws1 "print" >>. choice [printString; printNewline; printExpr] |>> AST.PrintStmt
    
let if_stmt =
    let if_then = skipStr_ws1 "if" >>. expr .>>. (skipStr_ws1 "then" >>. (many stmt))
    let if_then_endif = attempt if_then .>>? skipStr_ws "endif" |>> AST.IfThen
    let if_else = if_then .>> skipStr_ws1 "else" .>>. (many stmt) .>>? skipStr_ws "endif" |>> fun ((e, s1), s2) -> AST.IfThenElse (e, s1, s2)
    if_then_endif <|> if_else |>> AST.IfStmt

let nop = skipStr_ws ";"
let comment = skipStr_ws "//" >>. skipRestOfLine true >>. ws

stmtImp := many nop >>. many comment >>. (print <|> if_stmt <|> assign) .>> many comment

let program = ws >>. many stmt .>> eof;