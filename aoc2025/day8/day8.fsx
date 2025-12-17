#time
open System.IO

// So not pretty solution using naive union find that isn't functional
let parseInput (input:string) =
    input.Split(',') |> (fun ss -> ss.[0] |> int64, ss.[1] |> int64, ss.[2] |> int64)

let readFile path =
    File.ReadAllLines path
    |> Array.map parseInput
    |> Array.toList


//let data = readFile "test.txt"
let data = readFile "input.txt"

let dist ((a:int64,b:int64,c:int64),(x:int64,y:int64,z:int64)) =
    let x' = (float a - float x) ** 2.0
    let y' = (float b - float y) ** 2.0
    let z' = (float c - float z) ** 2.0
    x' + y' + z' |> sqrt

let organize (x, y) =
    let x' = if fst x <= fst y then x else y
    let y' = if fst x > fst y then x else y
    x',y'

let solvePart1 iterations data =
    let dataWithParentIdx = List.mapi (fun i x -> i,x) data
    let parentArray = Array.init (List.length dataWithParentIdx) id
    let allSortedByDistance =
        List.allPairs dataWithParentIdx dataWithParentIdx
        |> List.filter (fun (x,y) -> x <> y)
        |> List.map organize
        |> List.distinct
        |> List.sortBy (fun ((_,x),(_,y)) -> dist (x,y))

    let rec find i =
        let parent = parentArray.[i]
        if i = parent then i
        else find parent

    let union i j =
        let ip = find i
        let jp = find j
        parentArray.[jp] <- ip

    let rec solve stop n closest =
        if n >= stop then
            Array.map find parentArray 
        else
            match closest with
                | [] -> failwith "Empty list??"
                | x::[] -> failwith "Only one left??"
                | ((xi,x),(yi,y))::xs ->
                    union xi yi |> ignore
                    solve stop (n+1) xs

    solve iterations 0 allSortedByDistance


data
|> solvePart1 1000
|> Seq.countBy id
|> Seq.sortByDescending snd
|> Seq.take 3
|> Seq.map snd
|> Seq.reduce (*)
|> printfn "Part 1: %d"


// Part 2

let solvePart2 data =
    let dataWithParentIdx = List.mapi (fun i x -> i,x) data
    let parentArray = Array.init (List.length dataWithParentIdx) id
    let allSortedByDistance =
        List.allPairs dataWithParentIdx dataWithParentIdx
        |> List.filter (fun (x,y) -> x <> y)
        |> List.map organize
        |> List.distinct
        |> List.sortBy (fun ((_,x),(_,y)) -> dist (x,y))

    let rec find i =
        let parent = parentArray.[i]
        if i = parent then i
        else find parent

    let union i j =
        let ip = find i
        let jp = find j
        if parentArray.[jp] = ip then false
        else
            parentArray.[jp] <- ip
            true

    let flatten () =
        Array.iter (fun i -> parentArray.[i] <- find i) parentArray

    let rec solve lastPair closest =
        flatten ()
        if 1 = (Array.distinct parentArray |> Array.length) then
            lastPair
        else
            match closest with
                | [] -> lastPair
                | ((xi,x),(yi,y))::xs ->
                    if union xi yi then
                        let lastPair' = Some (x,y)
                        solve lastPair' xs
                    else
                        solve lastPair xs

    solve None allSortedByDistance

data
|> solvePart2
|> Option.get
|> (fun ((x1,_,_),(x2,_,_)) -> x1*x2)
|> printfn "Part 2: %d"
