open System.IO

let explode (s:string) : char list = [for c in s do yield c]

let readFile file =
    File.ReadAllText file
    |> explode

//let data = readFile "test.txt"
let data = readFile "input.txt"

let solve messageLength data =
    data
    |> List.mapi (fun i x -> i+1,x)
    |> List.windowed messageLength 
    |> List.map (fun l -> List.distinctBy (fun (i,x) -> x) l)
    |> List.filter (fun x -> List.length x = messageLength)
    |> List.head
    |> List.last
    |> fst

solve 4 data
|> printfn "Part 1: %d"

solve 14 data
|> printfn "Part 2: %d"

