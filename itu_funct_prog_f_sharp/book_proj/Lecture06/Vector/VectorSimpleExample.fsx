// Comple library: fsc -a VectorSimple.fsi VectorSimple.fs
#r @"/Users/nielshallenberg/Dropbox/Documents/Work/ITU/Course/KSFUPRO1KU-F2021/Lectures/Lec06/Vector/VectorSimple.dll"

open VectorSimple

let a = make(1.0,-2.0)

let b = make(3.0,4.0)

let c = 2.0 *. a -. b

coord c 

let d = c &. a

let e = norm b
