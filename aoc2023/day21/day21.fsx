open System.IO

let readFile file =
    File.ReadAllLines file
    |> List.ofArray
    |> List.map List.ofSeq
    |> List.mapi (fun i x -> List.mapi (fun j y -> (i,j),y) x)
    |> List.concat
    |> Map.ofList

//let data' = readFile "test.txt"
let data' = readFile "input.txt"

let start = Map.filter (fun _ v -> v = 'S') data' |> Map.toList |> List.map fst |> set 

let data =
    data'
    |> Map.filter (fun _ v -> v = '.' || v = 'S')


let step data (i,j) =
    let neighbors =
        [i-1,j;
         i+1,j;
         i,j-1;
         i,j+1] |> List.filter (fun (i,j) -> Map.containsKey (i,j) data)
    set neighbors


let steps data ss =
    Set.fold (fun acc x -> acc + (step data x)) (set []) ss

let rec nSteps data n ss =
    match n with
        | 0 -> ss
        | n ->
            steps data ss
            |> nSteps data (n-1)


nSteps data 64 start
|> Set.count
|> printfn "Part 1: %d"
