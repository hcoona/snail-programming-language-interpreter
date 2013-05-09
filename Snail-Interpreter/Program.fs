// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.
module Program

open FParsec

[<EntryPoint>]
let main argv = 
    let source = "x = 10;
y=20;
print \"the value of X is \"; print x; print newline;
print \"the value of Y is \"; print y; print newline;
print \"the sum of x and y is \"; print x + y; print newline; 
if x < y then //test x < y 
print \"x is smaller than y\"; // x is smaller 
else 
print \"x is bigger or equal than y\"; // x is not smaller 
endif
print newline;
"
    let ifelse = "if a then ; else endif"
//    let ast = run Parser.program source
    let ast = run Parser.if_stmt ifelse
    match ast with
    | Success (r, _, _) ->  printfn "The AST is shown below:"
                            printfn "%A" r
                            printfn ""
                            printfn "Begin to simulate:"
//                            Interpreter.evalProgram r
                            printfn "Simulation end."
    | Failure (es, _, _) -> printfn "Parse failed, the error information is shown below:"
                            printfn "%s" es
    0 // return an integer exit code