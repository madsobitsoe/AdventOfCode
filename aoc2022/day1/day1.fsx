open System.IO

// Parsing is hard and boring
let readFile file  =
    File.ReadAllText file
    |> (fun s -> s.Split "\n\n")
    |> List.ofArray
    |> List.map (fun s -> s.Split "\n")
    |> List.map List.ofArray
    |> List.map (List.filter (fun s -> s <> ""))
    |> List.filter (fun xs -> List.length xs >= 1)    
    |> List.map (List.map int)

let data = readFile "input.txt"

// Hella cool function composition
let part1 : int list list -> int = List.map List.sum >> List.max
let part2 : int list list -> int = List.map List.sum >> List.sort >> List.rev >> List.take 3 >> List.sum

// Easy to read, boring style
// let part1 (xs:int list list) =
//     List.map List.sum xs
//     |> List.max

part1 data |> printfn "Part 1: %d"

// Easy to read, boring style
// let part2 (xs:int list list) =
//     List.map List.sum xs
//     |> List.sort
//     |> List.rev
//     |> List.take 3
//     |> List.sum

part2 data |> printfn "Part 2: %d"
