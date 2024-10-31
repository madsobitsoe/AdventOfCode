open System.IO

// Part 1
let readFile file =
    File.ReadAllLines file
    |> Array.map (fun s -> s.Split ":")
    |> Array.map Array.tail
    |> Array.map (Array.map (fun s -> s.Replace( " ", ";")))
    |> Array.map (Array.map (fun s -> s.Split ";"))
    |> Array.map (Array.map (Array.filter ((<>) "")))
    |> Array.map Array.head
    |> Array.map (Array.map int)
    |> List.ofArray
    |> List.map List.ofArray
    |> (fun x -> List.head x, (List.skip 1 >> List.head) x)
    ||> List.zip 

//let data = readFile "test.txt"
let data = readFile "input.txt"

let rec computeDistance (t,d) ct =
    if ct <= 0 then []
    else
        let timeLeft = t - ct
        let distance = ct * timeLeft
        if distance > d then
            distance :: computeDistance (t,d) (ct-1)
        else
            computeDistance (t,d) (ct-1)


List.map (fun (t,d) -> computeDistance (t,d) t) data
|> List.map List.length
|> List.reduce (*)
|> printfn "Part 1: %d"


// Part 2

let readFile2 file =
    File.ReadAllLines file
    |> Array.map (fun s -> s.Split ":")
    |> Array.map Array.tail
    |> Array.map (Array.map (fun s -> s.Replace( " ", "")))
    |> Array.concat
    |> Array.map int64
    |> (fun x -> Array.head x, (Array.skip 1 >> Array.head) x)

//let p2data = readFile2 "test.txt"
let p2data = readFile2 "input.txt"

// Tail recursion baby!
let rec computeDistance2 (t:int64,d:int64) (ct:int64) acc =
    if ct <= 0L then acc
    else
        let timeLeft = t - ct
        let distance = ct * timeLeft
        if distance > d then
            computeDistance2 (t,d) (ct-1L) (distance :: acc)
        else
            computeDistance2 (t,d) (ct-1L) acc

computeDistance2 p2data (fst p2data) []
|> List.length
|> printfn "Part 2: %A"
