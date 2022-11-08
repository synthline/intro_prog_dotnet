let g xs = List.map (fun x -> x*x+1) xs

let isMember x xs = List.exists (fun y -> y=x) xs

let isMember x = List.exists ((=) x)

let disjoint xs ys =
  List.forall (fun x -> not (List.exists (fun y -> y=x) ys)) xs
  
let disjoint xs ys = List.forall (fun x -> not (isMember x ys)) xs
  
let disjoint xs ys = List.forall (fun x -> (not >> isMember) x ys) xs

let disjoint xs ys = List.forall (fun y -> (not >> isMember) y xs) ys

let disjoint xs = List.forall (fun y -> (not >> isMember) y xs)

let isMember2 xs x = List.exists ((=) x) xs
let disjoint2 xs = List.forall (not >> (isMember2 xs)) xs

let subset xs ys =
  List.forall (fun x -> List.exists (fun y -> x=y) xs) ys

let subset xs ys = List.forall (fun y -> isMember y xs) ys  

let subset xs ys = List.forall (isMember2 xs) ys

let inter xs ys = List.filter (fun x -> isMember x ys) xs
let inter xs ys = List.filter (isMember xs) ys
let inter xs = List.filter (isMember xs)

let insert x ys = if isMember x ys then ys else x::ys

let union xs ys =
  List.foldBack (fun x rs -> insert x rs) xs ys

let union xs ys = List.foldBack insert xs ys
  
let union xs = List.foldBack insert xs
let union = List.foldBack insert (* Error - Value restriction *)

    