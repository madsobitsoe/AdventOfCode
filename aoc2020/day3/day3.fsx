open System.IO

// Infinite rows
// No need for modulo indexing, we just take and skip what we need
let rec cycle xs = seq { yield! xs; yield! cycle xs }
let explode (s:string) : char list = [for c in s do yield c]
// Just reads the file into a list of char list
let readFile file  =
    File.ReadAllLines file
    |> List.ofArray
    |> List.map explode
    |> List.map cycle


// Load the input data and the test data
let data = readFile "input.txt"
let test = readFile "test.txt"

let rec goSkiing traj pos slope hits =
    match slope with
        | [] -> hits
        | s::ss ->
            let x,y = pos
            let tx,ty = traj
            let newPos = x+tx,y+ty
            // Is the current position a tree
            let isATree = Seq.take (x+1) s |> Seq.skip x |>  Seq.head |> (=) '#'
            let newHits = if isATree then hits + 1 else hits
            let newSlope = if List.length slope <= ty then [] else List.skip ty slope
            goSkiing traj newPos newSlope newHits
    

goSkiing (3,1) (0,0) data 0
|> printfn "Part 1 solution: %d"

// Part 2
// The answer is too large to fit in int32, so map the counts to int64 before reducing
let trajectories = [(1,1);(3,1);(5,1);(7,1);(1,2)]

List.map (fun t -> goSkiing t (0,0) data 0) trajectories
|> List.map int64
|> List.reduce (*)
|> printfn "Solution part 2: %A" 
