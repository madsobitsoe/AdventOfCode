open System.IO

let readFile file  =
    File.ReadAllText file
    |> (fun s -> s.Split(","))
    |> Array.map int
    |> List.ofArray

let data = readFile "input.txt"
let test = readFile "test.txt"

// Part 1
let alignatPart1 xs i = List.map (fun x -> abs (x - i)) xs |> List.sum

let alignerPart1 xs =
    let l,m = List.min xs, List.max xs
    List.map (alignatPart1 xs) [l..m] |> List.min


alignerPart1 data
|> printfn "Solution part 1: %d"

// Part 2
let rec getcost acc = function
    | 0 -> acc
    | n -> getcost (acc + n) (n-1)
    

let alignatPart2 xs i = List.map (fun x -> abs (x - i)) xs |> List.map (getcost 0) |> List.sum
let alignerPart2 xs =
    let l,m = List.min xs, List.max xs
    List.map (alignatPart2 xs) [l..m] |> List.min


alignerPart2 data
|> printfn "Solution part 2: %d"
