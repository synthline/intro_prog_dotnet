module test
// 6.1 (HR 6.2)
type Fexpr = 
    | Const of float
    | X
    | Add of Fexpr * Fexpr
    | Sub of Fexpr * Fexpr
    | Mul of Fexpr * Fexpr
    | Div of Fexpr * Fexpr
    | Sin of Fexpr
    | Cos of Fexpr
    | Log of Fexpr
    | Exp of Fexpr;;

let fexprToString expr =  
    match expr with
    | Const(value)      -> String.Format(CultureInfo.InvariantCulture, "{0:0.0######}", value)
    | Add(left, right)  -> toPostFix(left) + " " + toPostFix(right) + " +"
    | Sub(left, right)  -> toPostFix(left) + " " + toPostFix(right) + " -"
    | Mul(left, right)  -> toPostFix(left) + " " + toPostFix(right) + " *"
    | Div(left, right)  -> toPostFix(left) + " " + toPostFix(right) + " /"
    | Sin(expr)         -> toPostFix(expr) + " sin"
    | Cos(expr)         -> toPostFix(expr) + " cos"
    | Log(expr)         -> toPostFix(expr) + " log"
    | Exp(expr)         -> toPostFix(expr) + " exp"
    | expr              -> toPostFix(expr)
   

// 6.2 (HR 6.8)
(*
type Stack = S of float list
*)
type Instruction = | ADD | SUB | MULT | DIV | SIN
                   | COS | LOG | EXP | PUSH of float


let intpInstr =
match instruction, stack with
    | ADD, right::left::rest    -> (left + right)::rest
    | SUB, right::left::rest    -> (left - right)::rest
    | MUL, right::left::rest    -> (left * right)::rest
    | DIV, right::left::rest    -> (left / right)::rest
    | SIN, operand::rest        -> (sin operand)::rest
    | COS, operand::rest        -> (cos operand)::rest
    | LOG, operand::rest        -> (log operand)::rest
    | EXP, operand::rest        -> (exp operand)::rest
    | PUSH(value), stack        -> value::stack
    | _ -> failwith "Illegal stack state for operation"
//let intpProg ...

//let trans ...

// 6.3 (HR 7.2)
//type ComplexNumber ...
