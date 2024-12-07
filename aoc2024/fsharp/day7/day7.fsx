open System.IO

let (|Integer|_|) (str: string) =
   let mutable intvalue = 0L
   if System.Int64.TryParse(str, &intvalue) then Some(intvalue)
   else None

let getInt = function
    | Integer i -> i
    | x -> failwith <| sprintf "Failed parsing int: %A" x

let readFile file  =
    File.ReadAllText file
    |> (fun s -> s.Split "\n")
    |> Array.filter ((<>) "")
    |> List.ofArray

let parseLine (s:string) =
    let split = s.Split ": "
    let first = Array.head split |> getInt
    let numbers = split.[1].Split " " |> Array.map getInt |> List.ofArray
    first,numbers

// let data = readFile "test.txt"
let data = readFile "input.txt"

let rec canApply (expected:int64) (result:int64) xs =
    match xs with
        | [] -> expected = result
        | x::xs ->
            (canApply expected (result + x) xs) || (canApply expected (result * x) xs)

let rec canApply2 (expected:int64) (result:int64) xs =
    match xs with
        | [] -> expected = result
        | x::xs ->
            (canApply2 expected (sprintf "%d%d" result x |> getInt) xs) ||
            (canApply2 expected (result + x) xs) || (canApply2 expected (result * x) xs)

data
|> List.map parseLine
|> List.filter (fun (exp,xs) -> canApply exp (List.head xs) (List.tail xs))
|> List.map fst
|> List.sum
|> printfn "Part 1: %d"


data
|> List.map parseLine
|> List.filter (fun (exp,xs) -> canApply2 exp (List.head xs) (List.tail xs))
|> List.map fst
|> List.sum
|> printfn "Part 2: %d"

