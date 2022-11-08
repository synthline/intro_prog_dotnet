// Slide on Mutable record field
type 'a ref = {mutable contents: 'a}
let ref v = {contents = v}
let (!) r = r.contents
let (:=) r v = r.contents <- v

let a = ref 5
let b = a
b := 10
!a
!b


// Slides on mutable versus ref
let mutable a' = 5
let mutable b' = a'
b' <- 10
a'
b'


// Restrictions on polymorphic expressions, Section 8.5:

let mutable a = [] // Not allowed
let a = {contents = []} // Not allowed
let a = ref [] // Not allowed
let a() = {contents = []} // Allowed
1 :: a().contents // Allowed
'a' :: a().contents // Allowed
let a() = ref [] // Allowed
1 :: !a() // Allowed
'a' :: !a() // Allowed


Unchecked.defaultof<int> // Example default value

// Sequential composition
let mutable x = 0
let y = (x;23)
let y = (ignore x;23)
let y = (x |> ignore; 23)

//let ignore x = ()

// Arrays
let xs = [1;2]
let a = [|1;2|]

let a = Array.create 5 "a"
a.[2] <- "b"
a
a.[0]


// DFS

type color = White | Gray | Black

let dfs(V,adj: int list[]) =
  let color        = Array.create V White
  let pi           = Array.create V -1
  let d            = Array.create V -1
  let f            = Array.create V -1
  let time         = ref 0

  let rec visit u =
    color.[u] <- Gray ; time := !time + 1; d.[u] <- !time
    let rec h v = if color.[v] = White
                  then  pi.[v]  <- u
                        visit v
    List.iter h (adj.[u])
    color.[u] <- Black
    time      := !time + 1
    f.[u]     <- !time

  let mutable i = 0
  while i < V do
    if color.[i] = White
    then visit i
    i <- i + 1
  (d, f, pi)

let adj =
  Array.ofList [ [1;3];
                 [4];
                 [4;5];
                 [1];
                 [3];
                 [5]]

let g6 = (6,adj)
  
let (d,f,pi) = dfs(g6)
  

// Efficiency

let rec fact = function
    0 -> 1
  | n -> n*fact(n-1)

let rec fact n =
  match n with
    0 -> 1
  | n -> n*fact(n-1)

let fact n =
  let rec factA (n,m) =
    match (n,m) with
      (0,m) -> m
    | (n,m) -> factA (n-1, n*m)
  factA (n,1)

let rec factA = function
    (0,m) -> m
  | (n,m) -> factA (n-1, n*m)

let rec naiveRev = function
    [] -> []
  | x::xs -> naiveRev xs @ [x]

let rec revA = function
    ([], ys) -> ys
  | (x::xs, ys) -> revA(xs, x::ys)

let xs16 = List.init 1000000 (fun i -> 16)

#time

for i in xs16 do let _ = fact i in ()

for i in xs16 do let _ = factA (i,1) in ()

for i in xs16 do let _ = () in ()

let xs20000 = [1 .. 20000]

naiveRev xs20000

revA (xs20000, [])
#time

let rec bigList n = if n=0 then [] else 1::bigList (n-1)

bigList 120000

bigList 1300000

let rec bigListA n xs = if n=0 then xs else bigListA (n-1) (1::xs)

let xsVeryBig = bigListA 12000000 []

(* Do not wait for completion on machine configured with unlimited process memory. *)
let xsTooBig = bigListA System.Int32.MaxValue []

(* Using general iterative function *)

let rec factA (n,m) = if n <> 0 then factA(n-1, n*m) else m

let factW n =
  let ni = ref n
  let r = ref 1
  while !ni>0 do
    r := !r * !ni; ni := !ni-1
  !r

let rec revA (xs, ys) =
  if not (List.isEmpty xs)
  then revA(List.tail xs, (List.head xs)::ys)
  else ys

#time
for i in 1 .. 1000000 do let _ = factA (16,1) in ()

for i in 1 .. 1000000 do let _ = factW 16 in ()

let rec fib = function
    0 -> 0
  | 1 -> 1
  | n -> fib(n-1) + fib(n-2)

let rec fibC n c =
  match n with
    0 -> c 0
  | 1 -> c 1
  | n -> fibC (n-1) (fun res1 -> fibC (n-2) (fun res2 -> c(res1 + res2)))
    

fib 40

let rec itfib(n,a,b) =
  if n <> 0
  then itfib(n-1,a+b,a)
  else a

itfib(40,0,1)

let rec bigListC n c =
  if n=0 then c []
  else bigListC (n-1) (fun res -> c(1::res))

(* id is a built in function *)

#time
bigListC 270000 id
bigListC 26000000 id
bigList 260000
bigList 300000
bigList 2400000 // Too big.


type 'a BinTree =
    Leaf
  | Node of 'a BinTree * 'a * 'a BinTree

let rec count = function
    Leaf -> 0
  | Node(tl,n,tr) -> count tl + count tr + 1

let rec countC t c =
 match t with
     Leaf -> c 0
   | Node(tl,n,tr) ->
       countC tl (fun vl -> countC tr (fun vr -> c(vl+vr+1)))

let ex = Node(Node(Leaf,1,Leaf),2,Node(Leaf,3,Leaf))
countC ex id
(* 
(* L1 *)    countC (Node(Node(Leaf,1,Leaf),2,Node(Leaf,3,Leaf))) (fun1 x->x)
(* L2 *) ~> countC (Node(Leaf,1,Leaf)) (fun2 vl2 -> countC (Node(Leaf,3,Leaf)) (fun2 vr2 -> (fun1 x->x) (vl2+vr2+1)))

(* L3 *) ~> countC Leaf (fun3 vl3 -> countC Leaf (fun3 vr3 -> (fun2 vl2 -> countC (Node(Leaf,3,Leaf)) (fun2 vr2 -> (fun1 x->x) (vl2+vr2+1))) (vl3+vr3+1)))
         ~> (fun3 vl3 -> countC Leaf (fun3 vr3 -> (fun2 vl2 -> countC (Node(Leaf,3,Leaf)) (fun2 vr2 -> (fun1 x->x) (vl2+vr2+1))) (vl3+vr3+1))) 0
         ~> countC Leaf (fun3 vr3 -> (fun2 vl2 -> countC (Node(Leaf,3,Leaf)) (fun2 vr2 -> (fun1 x->x) (vl2+vr2+1))) (0+vr3+1))
         ~> (fun3 vr3 -> (fun2 vl2 -> countC (Node(Leaf,3,Leaf)) (fun2 vr2 -> (fun1 x->x) (vl2+vr2+1))) (0+vr3+1)) 0
         ~> (fun2 vl2 -> countC (Node(Leaf,3,Leaf)) (fun2 vr2 -> (fun1 x->x) (vl2+vr2+1))) (0+0+1)
         ~> (fun2 vl2 -> countC (Node(Leaf,3,Leaf)) (fun2 vr2 -> (fun1 x->x) (vl2+vr2+1))) 1

         ~> countC (Node(Leaf,3,Leaf)) (fun2 vr2 -> (fun1 x->x) (1+vr2+1))

(* L4 *) ~> countC Leaf (fun4 vl4 -> countC Leaf (fun vr4 -> (fun2 vr2 -> (fun1 x->x) (1+vr2+1)) (vl4+vr4+1)))
         ~> (fun4 vl4 -> countC Leaf (fun vr4 -> (fun2 vr2 -> (fun1 x->x) (1+vr2+1)) (vl4+vr4+1))) 0
         ~> countC Leaf (fun vr4 -> (fun2 vr2 -> (fun1 x->x) (1+vr2+1)) (0+vr4+1))
         ~> (fun vr4 -> (fun2 vr2 -> (fun1 x->x) (1+vr2+1)) (0+vr4+1)) 0
         ~> (fun2 vr2 -> (fun1 x->x) (1+vr2+1)) (0+0+1)
         ~> (fun2 vr2 -> (fun1 x->x) (1+vr2+1)) 1
         ~> (fun1 x->x) (1+1+1)
         ~> (fun1 x->x) 3
         ~> 3                      
*)