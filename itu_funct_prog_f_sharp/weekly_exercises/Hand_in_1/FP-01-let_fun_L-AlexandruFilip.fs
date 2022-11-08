module a1
// 1.1
let sqr n = n * n

// 1.2
let pow a b = System.Math.Pow(a, b)

let rec fact =
    function
    | (x, 0) -> 1.0
    | (x, n) -> x * fact (x, n - 1)


// 1.3 / HR 1.1
let g n = n + 4

// 1.4 / HR 1.2
let h (x: float, y: float) = System.Math.Sqrt((x * x) + (y * y))

// 1.5 / HR 1.4
let rec f =
    function
    | 0 -> 0
    | n -> n + f (n - 1)

// 1.6 / HR 1.5
let rec fib =
    function
    | 0 -> 0
    | 1 -> 1
    | n -> fib (n - 1) + fib (n - 2)

// // 1.7 / HR 1.6
let rec sum =
    function
    | (m, 0) -> 0
    | (m, n) -> m + n + sum (m, n - 1)

// // 1.8 / HR 1.7

// (System.Math.PI, fact -1) - float * int
// fact(fact 4) - int
// power(System.Math.PI, fact 2) - float * float
// (power, fact) - (float*float)*float


// // 1.9 / HR 1.8

// let a = 5
// let f a = a + 1
// let g b = (f b) + a

// f3 = 4
// g3 = 9


// // 1.10 Duplicate strings: dup:string -> string
let dup a: string = a + a

// // 1.11 Duplicate string n times.
let rec dupn (text: string) n =
    match n with
    |n < 0 -> ""
    |n > 0 -> text + dupn (text) (n - 1)


// Solution
	
// Missing evaluations of f 4, fib 4 and in 1.9, something like this:

// g 3 ~> 3+1 ~> 4+5 ~> 9

// 1.8
// power(System.Math.PI, fact 2) is float
// (power, fact) is (float * int -> float) * (int -> int)

// Error in 1.11, with a quick fix would look like this:
// let rec dupn (text: string) n =
//     match n with
//     |n when n <= 0 -> ""
//     |n -> text + dupn (text) (n - 1)