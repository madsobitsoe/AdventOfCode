open System.IO

let readFile file =
    File.ReadAllLines file
    |> List.ofArray
    |> List.map List.ofSeq
    |> List.mapi (fun i x -> List.mapi (fun j y -> (i,j),y) x)
    |> List.concat
    |> Map.ofList

let data' = readFile "test.txt"

let start = Map.filter (fun _ v -> v = 'S') data' |> Map.toList |> List.head

let data =
    data'
    |> Map.filter (fun _ v -> v = '.')

