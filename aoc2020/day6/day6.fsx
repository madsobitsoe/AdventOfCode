open System.IO

let explode (s:string) : char list = [for c in s do yield c]

let readFile file  =
    File.ReadAllText file
    |> (fun (s:string) -> s.Split("\n\n"))
    |> Array.map (fun (s:string) -> s.Replace(" ", "\n").Split("\n") |> Array.toList)
    |> Array.map (List.filter (fun s -> s <> ""))
    |> Array.toList
    |> List.filter (fun x -> x <> [])

    
let data = readFile "input.txt"
let test = readFile "test.txt" 

// part 1
List.map (List.map explode) data
|> List.map List.concat
|> List.map List.distinct
|> List.map List.length
|> List.sum
|> printfn "Part 1 solution: %d"

// Part 2
let getUnion xs =
    let len = List.length xs
    let all = List.concat xs
    let counts = List.countBy id all
    List.filter (fun (_,c) -> c = len) counts
    |> List.map (fun (c,_) -> c)

List.map (List.map explode) data
|> List.map getUnion
|> List.map List.distinct
|> List.map List.length
|> List.sum
|> printfn "Part 2 solution: %d"

