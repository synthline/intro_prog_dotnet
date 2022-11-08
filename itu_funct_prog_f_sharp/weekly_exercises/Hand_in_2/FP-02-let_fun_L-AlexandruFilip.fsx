module a2

// 2.1 time difference:
let timediff (h0, m0) (h1, m1) = ((h1 * 60) + m1) - ((h0 * 60) + m0)

// 2.2 function minutes
let minutes (h, m) = (h * 60) + m

// 2.3 / HR 2.2
let rec pow (s: string, n: int) =
    match n with
    | n when n <= 0 -> ""
    | n -> s + pow (s, n - 1)

// 2.4 / HR 2.8
let rec bin (a, b) =
    match b with
    | b when b = 0 -> 1
    | b when a = b -> 1
    | b -> bin (a - 1, b - 1) + bin (a - 1, b)


// 2.5 / HR 2.9

// 1. Determine the type of f.
// A: the resulting type is of int.

// 2. For which arguments does the evaluation of f terminate?
// A: When the first number (x) will be zero.
// Q: Could I also answer in this case something like: When the first argument in the tuple (or even say pair) is xero? :D


// 3. Write the evaluation steps for f(2,3).
// A:
// ~> f(1, (2*3))  - clause 2 of the function since in this case x <> 0
// ~> f(1*6)
// ~>6

// 4. What is the mathematical meaning of f(x, y)?
// A: it is a multiplication


// 2.6 / HR 2.10

let rec fact =
    function
    | 0 -> 1
    | n -> n * fact (n - 1)

let test (c, e) = if c then e else 0

// 1. What is the type of test?
// A: The resulting type is of int.

// 2. What is the result of evaluating test(false, fact(-1))?
// A: I get a stack overflow, and I'm guessing this has to do with a fact(-1)? :D

// 3. Compare this with the result of evaluating
// if false then fact -1 else 0
// A:  the answer is 0 :D


// 2.7 / HR 2.13 Curry and Uncurry
let curry f x y = f (x, y)

let uncurry g (x, y) = g x y
