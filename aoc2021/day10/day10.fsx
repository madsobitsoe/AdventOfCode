open System.IO
open System.Text.RegularExpressions

let explode (s:string) : char list = [for c in s do yield c]

let readFile file =
    File.ReadAllLines file
    |> List.ofArray
    |> List.map explode

let data = readFile "input.txt"
let test = readFile "test.txt"


// Part 1
let points = function
    | ')' -> Some 3
    | ']' -> Some 57
    | '}' -> Some 1197
    | '>' -> Some 25137
    | _ -> Some 0 

let rec expect closers = function
    | [] -> None
    | '('::xs -> expect (')'::closers) xs
    | '['::xs -> expect (']'::closers) xs
    | '{'::xs -> expect ('}'::closers) xs
    | '<'::xs -> expect ('>'::closers) xs
    | x::xs ->
        if List.head closers = x then expect (List.tail closers) xs
        else Some x

List.map (expect []) data
|> List.map (fun x -> match x with None -> Some 0 | Some x -> points x)
|> List.reduce (Option.map2 (+))
|> (fun x ->
    match x with
    | Some x -> printfn "Solution part 1: %d" x
    | _ -> printfn "Error in solution for part 1")

// Part 2
let rec expect' closers = function
    | [] -> Some closers 
    | '('::xs -> expect' (')'::closers) xs
    | '['::xs -> expect' (']'::closers) xs
    | '{'::xs -> expect' ('}'::closers) xs
    | '<'::xs -> expect' ('>'::closers) xs
    | x::xs ->
        if List.head closers = x then expect' (List.tail closers) xs
        else None

let points' = function
    | ')' ->  1L
    | ']' ->  2L
    | '}' ->  3L
    | '>' ->  4L
    | _ -> failwith "Illegal char"


let pointsForLine = List.fold (fun acc x -> acc * 5L + (points' x)) 0L
let findMiddle xs = List.length xs / 2 |> List.skip <| List.sort xs |> List.head

List.map (expect' []) data
|> List.filter (fun x -> x.IsSome)
|> List.map (fun x -> x.Value)
|> List.map pointsForLine
|> findMiddle
|> printfn "Solution part 2: %d" 

