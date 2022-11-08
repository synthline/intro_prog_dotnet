#r @"/Users/nielshallenberg/Dropbox/Documents/Work/ITU/Course/KSFUPRO1KU-F2021/Lectures/Lec06/Queue/Queue.dll" 

let q0 = Queue.empty

let q0 = Queue.empty : Queue.Queue<int>

let q1 = Queue.put 1 q0

let q2 = Queue.put 2 q1

let (x,q3) = Queue.get q2


(* Equality does not work *)
let qnew = Queue.put 2 q0
let r = qnew = q3

