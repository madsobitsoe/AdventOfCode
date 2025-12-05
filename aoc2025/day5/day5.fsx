open System.IO

let readFile file =
    File.ReadAllText file
    |> (fun (s:string) -> s.Split("\n\n"))
    |> List.ofArray
    |> (fun xs -> xs.[0],xs.[1])
    |> (fun (a,b) -> a.Split ("\n") |> List.ofArray, b.Split("\n") |> List.ofArray)
    |> (fun (a,b) -> List.filter ((<>) "") a, List.filter ((<>) "") b)


let data = readFile "input.txt"
//let data = readFile "test.txt"

let ranges',ingredients' = data
let ingredients = List.map int64 ingredients'
let ranges =
    ranges'
    |> List.map (fun (s:string) -> s.Split ("-") |> (fun xs -> xs.[0] |> int64,xs.[1] |> int64))

let isFresh x = List.exists (fun (a,b) -> a <= x && x <= b) ranges 

// Part 1 
List.filter isFresh ingredients
|> List.length
|> printfn "Part 1: %d"


// Part 2
let sorted = List.sort ranges

let rec join acc xs =
    match xs with
        | [] -> acc |> List.rev
        | x::[] -> x::acc |> List.rev
        | (a,b)::(c,d)::xs -> 
            if a <= c && b >= d then
                join acc ((a,b)::xs)
            else if a <= c && b >= c && b < d then
                join acc ((a,d)::xs)
            else join ((a,b)::acc) ((c,d)::xs)

let count acc (a,b) =
    let inRange =
        if a = b then 1L
        else b + 1L - a
    acc + inRange

sorted
|> join []
|> List.fold count 0L
|> printfn "Part 2: %d"
    
