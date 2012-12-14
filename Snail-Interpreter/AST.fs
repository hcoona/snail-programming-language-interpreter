module AST

type id = string
type uniOp = Neg
type binOp =
    | Plus | Minus | Times | Division
    | LT | LEQ | GT | GEQ | EQ | NEQ

type expr = 
    | INT of int
    | ID of id
    | UniOpExpr of uniOp * expr
    | BinOpExpr of binOp * expr * expr

type stmt =
    | AssignStmt of assign_stmt
    | PrintStmt of print_stmt
    | IfStmt of if_stmt
and assign_stmt = Assign of id * expr
and print_stmt =
    | PrintExpr of expr
    | PrintNewline
    | PrintString of string
and if_stmt =
    | IfThen of expr * stmt list
    | IfThenElse of expr * stmt list * stmt list

type program = stmt list