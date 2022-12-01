open System.IO

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

let part1 (xs:int list list) =
    List.map List.sum xs
    |> List.max

part1 data |> printfn "Part 1: %d"

let part2 (xs:int list list) =
    List.map List.sum xs
    |> List.sort
    |> List.rev
    |> List.take 3
    |> List.sum

part2 data |> printfn "Part 2: %d"
