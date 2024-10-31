open System.IO

let readFile file =
    File.ReadAllLines file
    |> Array.map (fun s -> s.Split " ")
    |> List.ofArray
    |> List.map List.ofArray
    |> List.map (List.map int64)


//let data = readFile "test.txt"
let data = readFile "input.txt"

let rec extrapolate acc (xs:int64 list) =
    let diff =
        List.pairwise xs
        |> List.map (fun (a,b) -> b - a)
    let last = List.last xs
    if List.forall ((=) 0L) diff then last + acc
    else extrapolate (acc + last) diff

let solve data =
    List.map (extrapolate 0L) data
    |> List.sum
    
solve data
|> printfn "Part 1: %A"


List.map List.rev data
|> solve
|> printfn "Part 2: %A"
