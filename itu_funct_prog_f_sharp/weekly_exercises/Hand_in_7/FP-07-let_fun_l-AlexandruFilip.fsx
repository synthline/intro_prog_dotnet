module a8

(* Assignment 7.1, HR 9.1 *)
(* Not covered by Code Judge *)
let xs = [1;2]

let rec g = function
    0 -> xs
  | n -> let ys = n::g(n-1)
         List.rev ys

g 2

(* Assignment 7.2, HR 9.3 *)

let rec sum(m,n) = 
  let mutable result = m
  for value in 1..n do
   result <- result + (m + value)
  result

(* Assignment 7.3, HR 9.4 *)
let length xs = 
  let rec length counter xs = 
    if (not (List.isEmpty xs)) then length (counter + 1) (List.tail xs)
    else counter           
  length 0 xs

(* Example *)
length [1]

(* Assignment 7.4, HR 9.6 *)

let rec facC n c  = failwith "not yet"
  
(* Assignment 7.5, HR 8.6 *)
let rec fib n =  
  let rec fibonacci (``fn-1``, ``fn-2``) = function
  | zeroOrNeg when zeroOrNeg <= 0 -> None
  | 1 -> Some ``fn-2``
  | 2 -> Some ``fn-1``        
  | n -> fibonacci (``fn-1`` + ``fn-2``, ``fn-1``) (n - 1) 
  
  fibonacci (1, 0) n

(* Example *)
fib 4

(* Assignment 7.6, HR 9.7 *)
let rec fibA n n1 n2 = 
  match n with
  |0 -> n2
  |1 -> n1
  |n -> fibA (n-1) (n1+n2) n1

[1..10000] |> List.iter (fun _ ->  [0..33] |> List.map (fun i ->fibA i 1 0) |> ignore)

(* Example *)
fibA 10 0 1

let rec fibC n c = 
  match n with
  | 0 -> c 0
  | 1 -> c 1
  | n -> fibC (n - 2) (fun a -> fibC (n-1) (fun b -> c (a + b)))

[1..10] |> List.iter (fun _ ->  [0..33] |> List.map (fun i -> fibC i id) |> ignore)

(* Example *)
fibC 10 id