module test

type 'a BinTree =
    Leaf
    | Node of 'a * 'a BinTree * 'a BinTree

// 5.1

let rec inOrder = function
    | Leaf -> []
    | Node (x,tl,tr) -> (inOrder tl) @ [x] @ ( inOrder tr)

// 5.2  

let rec mapInOrder f  = function
     | Leaf -> Leaf
     | Node (x,tl,tr) -> let lt = mapInOrder f tl
                         let nv = f x
                         let rt = mapInOrder f tr
                         Node (nv, lt, rt)

// // 5.3

let rec foldInOrder f acc =
    function 
    | Leaf -> acc
    | Node (x,tl,tr) -> let lt = foldInOrder f acc tl
                        let nv = f x lt
                        foldInOrder f nv tr     


//5.4 + 5.5
type aExp =                     (* Arithmetical expressions *)
    | N of int                  (* numbers *)
    | V of string               (* variables *)
    | Add of aExp * aExp        (* addition *)
    | Mul of aExp * aExp        (* multiplication *)
    | Sub of aExp * aExp        (* subtraction *)

type bExp =                     (* Boolean expressions *)
    | TT                        (* true *)
    | FF                        (* false *)
    | Eq of aExp * aExp         (* equality *)
    | Lt of aExp * aExp         (* less than *)
    | Neg of bExp               (* negation *)
    | Con of bExp * bExp        (* conjunction *)

type stm =                      // statements
    | Ass of string * aExp      // assignment
    | Skip
    | Seq of stm * stm          // sequential composition
    | ITE of bExp * stm * stm   // if-then-else
    | While of bExp * stm       // while
    | IT of bExp * stm         // if-then
    | RU of  bExp * stm        //  repeat until

let rec A a s =
    match a with
    | N n -> n
    | V x -> Map.find x s
    | Add(a1, a2) -> A a1 s + A a2 s
    | Mul(a1, a2) -> A a1 s * A a2 s
    | Sub(a1, a2) -> A a1 s - A a2 s;;


let rec B b s =
    match b with
    | TT -> true
    | FF -> false
    | Eq(b1, b2) -> A b1 s = A b2 s
    | Lt(b1, b2) -> A b1 s < A b2 s 
    | Neg b -> not (B b s)
    | Con(b1, b2) -> B b1 s && B b2 s

let rec I stm s =
    match stm with
    | Ass(x,a) -> Map.add x (A a s) s
    | Skip -> s
    | Seq(stm1, stm2) -> I stm1 s |> I stm2
    | ITE(b,stm1,stm2) -> if B b s then I stm1 s else I stm2 s
    | While(b, stmn) -> if B b s then I stmn s |> I stm else s
    | IT (b, stm1) -> if B b s then I stm1 s else I Skip s 
    | RU (b, stmz) -> I stmz s |> if B b s then I stm else id  


// // // Example 0
// let stmt0 = Ass("res",(Add(N 50, N 30)))
// // Example 1
// let stmt1 = IT(Mul(Sub(N 30, N 50),Add(N 50, N 30))< N 20 , Skip )
// let state1 = Map.empty
// // Example 2
// let stmt2 = ITE(EQ(Add(N 50, N 30), Sub(N 30, N 50)), Ass("eq", V "is true" ), Ass("eq", V "not true" ) )
// let state2= Map.empty
// // Example 3
// let stmt3 = IT(Lt(Add(N 50, N 30), Add(N 80, N 30), Skip ))
// let state3 = Map.empty
// // Example 4
// let stmt4 = Mul(Sub(N 10, N 50),Add(N 50, N 50)
// let state4 = Map.empty
// // Example 5
// let stmt5 = While(Conj(TT,FF), Ass("loop", V "4 ever" )   )
// let state5 = Map.empty


