open System.IO
open System.Text.RegularExpressions

// Parsing is hard and boring
let readFile file  =
    File.ReadAllText file
    |> (fun s -> s.Split "\n")
    |> List.ofArray    

let data = readFile "input.txt"

// let data = readFile "test.txt"


let explode (s:string) : string list = [for c in s do yield c] |> List.map string

let isDigit (s:string) = System.Text.RegularExpressions.Regex("[0-9]").IsMatch s

let concat (s:string list) : string =
    let first = List.head s
    let last = List.last s
    List.head s + List.last s


let part1 data =
    List.map explode data
    |> List.map (List.filter isDigit)
    |> List.filter (fun l -> List.length l > 0)
    |> List.map concat
    |> List.map int
    |> List.sum



// Part 2
let s2i (s:string) =
    s.Replace ("eight", "e8ight")
    |> (fun s -> s.Replace ("one", "o1ne"))
    |> (fun s -> s.Replace ("four", "f4our"))
    |> (fun s -> s.Replace ("five", "f5ive"))
    |> (fun s -> s.Replace ("six", "s6ix"))
    |> (fun s -> s.Replace ("nine", "n9ine"))
    |> (fun s -> s.Replace ("seven", "s7even"))
    |> (fun s -> s.Replace ("two", "t2wo"))
    |> (fun s -> s.Replace ("three", "t3hree"))

let part2 data =
    List.map s2i data
    |> List.map explode
    |> List.map (List.filter isDigit)
    |> List.filter (fun l -> List.length l > 0)
    |> List.map concat
    |> List.map int
    |> List.sum


part1 data
|> printfn "Part1: %d"

part2 data
|> printfn "Part2: %d"
