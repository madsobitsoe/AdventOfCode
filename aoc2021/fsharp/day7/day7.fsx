open System.IO

let readFile file  =
    File.ReadAllText file
    |> (fun s -> s.Split(","))
    |> Array.map int
    |> List.ofArray

let data = readFile "input.txt"
let test = readFile "test.txt"

let diff i = ((-) i) >> abs

// Part 1
let alignatPart1 xs i = List.map (diff i) xs |> List.sum

let alignerPart1 xs =
    let l,m = List.min xs, List.max xs
    List.map (alignatPart1 xs) [l..m] |> List.min


alignerPart1 data
|> printfn "Solution part 1: %d"

// Part 2
let cost n = n * (n+1) / 2

let alignatPart2 xs i = List.map (diff i >> cost) xs |> List.sum
let alignerPart2 xs =
    let l,m = List.min xs, List.max xs
    List.map (alignatPart2 xs) [l..m] |> List.min

alignerPart2 data
|> printfn "Solution part 2: %d"
