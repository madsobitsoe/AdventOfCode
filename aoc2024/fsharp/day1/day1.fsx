open System.IO

// Parsing is hard and boring
let readFile file  =
    File.ReadAllText file
    |> (fun s -> s.Split "\n")
    |> Array.map (fun s -> s.Split " ")
    |> Array.map List.ofArray
    |> List.ofArray
    |> List.map (List.filter ((<>) ""))
    |> List.filter (fun x -> x <> [])
    |> List.map (List.map int)
    |> List.map (fun (xs:int list) -> xs.[0], xs.[1])
    |> List.unzip
    
let part1 (data: int list * int list) =
    data
    |> fun (a,b) -> List.sort a, List.sort b
    ||> List.map2 (fun a b -> abs (a - b))
    |> List.sum


//let data = readFile "test.txt"
let data = readFile "input.txt"


let f x map =
    match Map.tryFind x map with
        | None -> 0
        | Some x -> x

let part2 data =
    let left = fst data
    let right =
        data
        |> snd
        |> List.countBy id
        |> Map
    List.map (fun x -> f x right |> (*) x) left
    |> List.sum 

part1 data
|> printfn "Part 1: %d"

part2 data
|> printfn "Part 2: %d"
