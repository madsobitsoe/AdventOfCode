open System.IO

let readFile file  =
    File.ReadAllLines file
    |> List.ofArray
    |> List.map int

let data = readFile "input.txt"

// Part 1
let rec countIncs acc  = function
    | x::y::ys ->
        if x < y then countIncs (acc+1) (y::ys)
        else countIncs acc (y::ys)
    | _ -> acc

countIncs 0 data |> printfn "Part 1 solution: %d"

// Part 2
let rec countIncsWindow acc = function
    | a::b::c::d::xs ->
        let sumOne = a + b + c
        let sumTwo = b + c + d
        if sumOne < sumTwo then countIncsWindow (acc+1) (b::c::d::xs)
        else countIncsWindow acc (b::c::d::xs)
    | _ -> acc

countIncsWindow 0 data |> printfn "Part 2 solution: %d"
