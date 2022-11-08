module a2

// Exercise 2.7 - explode - string to char list
let explode (s:string) = s.ToCharArray() |> List.ofArray

let rec explode2 (s:string)  = 
    match s with
    |"" -> []
    |s -> s.[0] :: explode2 (s.Remove(0,1))

// Exercise 2.8 - implode - char list to string

let implode list = List.foldBack ( fun x y -> string x + y ) list "" 

let implode2 list = List.foldBack ( fun x y -> string x + y ) list "" 

let implodeRev list = List.fold  ( fun s a -> string a + s ) "" list

// Exercise 2.9 - toUpper

let toUpper (s:string) = explode s |> List.map System.Char.ToUpper |> implode2  

// explode >> map >> implode

let toUpper1 (s:string) =  ((explode >> List.map System.Char.ToUpper) >> implode2) s

// let toUpper2 (s:string) = implode (explode s |> List.map(fun x -> (Char.ToUpper(x))));;

let toUpper2 (s:string) =  (|>) ( (<<) (List.map System.Char.ToUpper) explode s ) implode2  

// Exercise 2.10 - palindrome - treating empty strings as palindromes too.

let rec palindrome (s:string) = if (s |> explode |> implode2) = (s |> explode |> implodeRev) then true else false

// Exercise 2.11 - ack

let rec ack t  = 
        match t with
        | (x,y) when x = 0 ->  int y + 1 
        | (x,y) when x > 0 && y = 0 -> ack (x-1, y)
        | (x,y) when x > 0 && y > 0 -> ack (x-1, ack(x,y-1))
        | _ -> failwith "no good biznis"

    // ack(3, 11) = 16381

// Exercise 2.12 - time

let time f = 
    let start = System.DateTime.Now in
    let res = f () in
    let finish = System.DateTime.Now in
    (res, finish - start)

let timeArg1 f a = time (fun () -> f a)  

// timeArg1 f a : (’a -> ’b) -> ’a -> ’b * TimeSpan

// Exercise 2.13 - HR 5.4 - downTo2 f n e - pattern matching -  go back to the book
 
let rec downto1 f (n, e) = 
    match n with
    | n when n < 0 -> e
    | _ -> downto1 f ( n-1, f(n, e)) 
       

// factorial function using downto1 for recursion. - use down to 1 as a function for 

let rec fact n = downto1 (fun (a,b) -> a * b) (n,1)

let buildList g n = failwith "no implement" 
