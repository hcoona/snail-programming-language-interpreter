module Interpreter

let var_table = new System.Collections.Generic.Dictionary<string, int>(64)

let rec evalStmt =     
    let evalAssignStmt = function
        | AST.Assign (id, expr) -> var_table.Item id <- evalExpr expr
    let evalPrintStmt = function
        | AST.PrintNewline -> printfn ""
        | AST.PrintString str -> printf "%s" str
        | AST.PrintExpr expr -> evalExpr expr |> printf "%d"
    let evalIfStmt = function
        | AST.IfThen (expr, stmt_list) ->   if evalExpr expr = 1 then
                                                List.iter evalStmt stmt_list
        | AST.IfThenElse (expr, s1, s2) ->  if evalExpr expr = 1 then
                                                List.iter evalStmt s1
                                            else
                                                List.iter evalStmt s2
    function
    | AST.AssignStmt assign_stmt -> evalAssignStmt assign_stmt
    | AST.PrintStmt print_stmt -> evalPrintStmt print_stmt
    | AST.IfStmt if_stmt -> evalIfStmt if_stmt
and evalExpr =
    let boolToInt32 p = if p then 1 else 0
    function
    | AST.INT value -> value
    | AST.ID id -> var_table.Item id
    | AST.UniOpExpr (AST.Neg, expr) -> - evalExpr expr
    | AST.BinOpExpr (AST.Plus, e1, e2) -> (evalExpr e1) + (evalExpr e2)
    | AST.BinOpExpr (AST.Minus, e1, e2) -> (evalExpr e1) - (evalExpr e2)
    | AST.BinOpExpr (AST.Times, e1, e2) -> (evalExpr e1) * (evalExpr e2)
    | AST.BinOpExpr (AST.Division, e1, e2) -> (evalExpr e1) / (evalExpr e2)
    | AST.BinOpExpr (AST.LT, e1, e2) -> (evalExpr e1) < (evalExpr e2) |> boolToInt32
    | AST.BinOpExpr (AST.LEQ, e1, e2) -> (evalExpr e1) <= (evalExpr e2) |> boolToInt32
    | AST.BinOpExpr (AST.GT, e1, e2) -> (evalExpr e1) > (evalExpr e2) |> boolToInt32
    | AST.BinOpExpr (AST.GEQ, e1, e2) -> (evalExpr e1) >= (evalExpr e2) |> boolToInt32
    | AST.BinOpExpr (AST.EQ, e1, e2) -> (evalExpr e1) = (evalExpr e2) |> boolToInt32
    | AST.BinOpExpr (AST.NEQ, e1, e2) -> (evalExpr e1) <> (evalExpr e2) |> boolToInt32

let rec evalProgram = function
    | [] -> ()
    | hd :: tl -> evalStmt hd; evalProgram tl