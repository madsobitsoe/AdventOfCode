open System.IO

let readFile file =
    File.ReadAllLines file
    |> Array.map (fun s -> s.Split " ")
    |> Array.map (fun x -> Array.head x, (Array.tail >> Array.head) x, (Array.tail >> Array.tail >> Array.head) x)
    |> List.ofArray
    |> List.map (fun (dir,count,color) -> dir, int count, color)


    
let data = readFile "test.txt"

// let toLine start instruction =
//    let dir,count,color = instruction
//    let x,y = start
//    match dir with
//        | "R" ->
//            let l = List.map (fun j -> (x,j),color) [y..count]
//            let next = List.last l
//            next,l
//        | "L" ->
//            let l = List.map (fun j -> (x,j),color) [y .. -1 .. 0-count]
//            let next = List.last l
//            next,l
//        | "U"  ->
//            let l = List.map (fun i -> (i,y),color) [x .. -1 .. 0-count]
//            let next = List.last l
//            next,l
//        | "D" ->
//            let l = List.map (fun i -> (i,y),color) [x .. count]
//            let next = List.last l
//            next,l


// List.head data |> toLine (0,0)

// List.fold (fun acc x -> let acc' = (snd acc) @ (toLine (fst acc) x) in List.last acc',acc') ((0,0),[]) data

// let rec makeMap instructions map =
    
