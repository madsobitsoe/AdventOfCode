#time
open System.IO
open System.Text.RegularExpressions

let explode (s:string) : string list = [for c in s do yield c] |> List.map string
let isDigit (s:string) = System.Text.RegularExpressions.Regex("[0-9]").IsMatch s

// Parsing is hard and boring
let readFile file  =
    File.ReadAllText file
    |> (fun s -> s.Split "\n")
    |> Array.map (fun s -> s.Split " ")
    |> Array.filter (fun a -> a <> [||] && a <> [|""|])
    |> Array.map (Array.map int)
    |> List.ofArray
    |> List.map List.ofArray


//let data = readFile "test.txt"
let data = readFile "input.txt"


let inOrder xs =
    xs = List.sort xs || xs = List.sortDescending xs


let part1 data =
    List.filter inOrder data

let rec diffs xs =
    match xs with
        | a::b::[] -> abs (a - b)::[]
        | a::b::xs -> abs (a-b)::diffs (b::xs)
        | [] -> []

let maxIncrease x = x >= 1 && x <= 3

part1 data
|> List.map diffs
|> List.filter (List.forall maxIncrease)
|> List.length
|> printfn "Part 1: %d"


let f' data =
    List.filter inOrder data
    |> List.map diffs
    |> List.filter (List.forall maxIncrease)

let f xs n = List.removeAt n xs

let rec allReports xs =
    List.map (f xs) [0..List.length xs - 1]

data
|> List.map allReports
|> List.map f'
|> List.filter ((<>) [])
|> List.length
|> printfn "Part 2: %d"

