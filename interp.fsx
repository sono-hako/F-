System.Environment.set_CurrentDirectory __SOURCE_DIRECTORY__;;
#load "parser.fsx"

open Parser.Parse
open Parser.Lex


let rec subst e x t = 
    match e with
    | FUN(x, e1)   -> FUN(x, (subst e1 x t))
    | REC(x, e1)   -> REC(x, (subst e1 x t))
    | APP(e1, e2) -> APP((subst e1 x t), (subst e2 x t))
    | ID(x)       -> t
    | e           -> e

let rec interp = function
| NUM n        -> NUM n         
| BOOL true    -> BOOL true     
| BOOL false   -> BOOL false    
| SUCC         -> SUCC          
| PRED         -> PRED          
| ISZERO       -> ISZERO        
| ID (e1)      -> ERROR (sprintf "unbound ID %A" e1)
| ERROR s      -> ERROR (sprintf "err %A" s)
| FUN (e1, e2) -> FUN(e1, e2)   //R(9)
| REC (e1, e2) -> interp(subst e2 e1 (REC(e1, e2))) //--NOT WORKING CORRECTLY
| IF (b, e1, e2) ->              //R(4), R(5)
    match(interp b) with
    | (ERROR s)     -> ERROR s
    | (BOOL true)   -> interp e1  //R(4) b => true    e1 => v
    | (BOOL false)  -> interp e2  //R(5) b => false   e2 => v
    | (_)           -> ERROR (sprintf "if: argument must be type (bool)")
| APP (e1, e2) ->
    match (interp e1, interp e2) with
    | (ERROR s, _)  -> ERROR s        // ERRORs are propagated         
    | (_, ERROR s)  -> ERROR s        // ERRORs are propagated         
    | (SUCC, NUM n) -> NUM (n+1)                       
    | (SUCC, _)     -> ERROR (sprintf "succ: needs argument must be type (int)") 
    | (PRED, NUM 0) -> NUM 0          
    | (PRED, NUM n) -> NUM (n-1)      
    | (PRED, _)     -> ERROR (sprintf "pred: needs argument must be type (int)")//R(7)
    | (ISZERO, NUM 0) -> BOOL true    
    | (ISZERO, NUM _) -> BOOL false   
    | (ISZERO, _)     -> ERROR (sprintf "iszero: argument must be type (int)")  
    | (FUN(x, e), t) -> interp (subst e x t)//R(10)  e1 => (fun x -> e)      e2 => v1    e[x:=v1] => v2
    | (_,_)         -> ERROR (sprintf "incorrect syntax")

let interpfile filename = filename |> parsefile |> interp

let interpstr sourcecode = sourcecode |> parsestr |> interp
