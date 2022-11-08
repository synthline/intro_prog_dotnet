module a2
// Exercise 3.1 downTo + downTo2
let rec downTo n = if n > 0 then  n::downTo(n-1) else []
let f= 0;;

let rec downTo2 n= 
  match n with
  | 0 -> []
  | n -> n::downTo2(n-1)

// Exercise 3.2 removeOddIdx

let rec removeOddIdx (xs: int list) = 
  match xs with 
  | [] -> []
  | [x0] -> [x0]
  | x0::x1::xs -> x0:: removeOddIdx xs


// Exercise 2.3 combinePair

let rec combinePair (xs: int list) = 
  match xs with
  | [] -> []
  | [x0] -> []
  | [x0;x1;x2] -> [(x0,x1)]
  | x0::x1::xs -> (x0,x1):: combinePair xs

// Exercise 2.4 - HR 3.2 - British currency

let all2Pence (a,b,c) = a*240 + b*12 + c  

let back2Init a = 
    let p = if a < 240 then a else a/240
    let pe = if (a%240) < 12 then (a%240) else (a%240)/12 
    let s = (a%240)%12
    if a < 0 then failwith "no good biznis" else (p, pe, s)


let (^+^) a b = all2Pence a + all2Pence b |> back2Init 
let (^-^) a b = if all2Pence a > all2Pence b then all2Pence a - all2Pence b |> back2Init else failwith "No good biznis"

type Money =
    { pound: int
      shilling: int
      pence: int }

let Convert {pound=p; shilling=sh; pence=pc} = (p,sh,pc)
let ConvertB (p,sh,pc) = {pound=p; shilling=sh; pence=pc}

// Money record addition
let (|+|) a b = (^+^) (Convert a) (Convert b) |> ConvertB
// Money record subtraction
let (|-|) a b = (^-^) (Convert a) (Convert b) |> ConvertB

// Exercise 2.5 - HR 3.3 - Complex numbers

//      1. Declare infix for addition and multiplication
let (.+) (a: float, b: float) (c: float, d: float) = (a+c, b+d)

let (.*) (a: float, b: float) (c: float, d: float) = (a*c - b*d, b*c + a*d)

//      2. Declare infix for subtraction and division
let (.-) (a: float, b: float) (c: float, d: float) = (a + -c,b + -d)

let (./) (a,b) (c,d) = (.*) (a,b) (c/(c*c+d*d),-d/(c*c+d*d) )


//      3. Use 'let' expressions in division to avoid repeated evals
let (../) (a: float, b: float) (c: float, d: float) =  (./) (a,b) (c,d)



// You shouldn't take the inverse of (a,b). Neither for subtraction nor multiplication. substraction should be = (a + -c, b + -d)  and division should be equal to (a,b) .* 1/(c,d).
// This however is less important, and we are happy that almost everyone showed they are able to make infix operations.

// Exercise 2.6 - HR 4.4 - altSum -> HR page 76

// function alternating between adding and subtracting the contents of a list.

let rec altsum = function
| [] -> 0
| x::xs -> x - altsum xs;;