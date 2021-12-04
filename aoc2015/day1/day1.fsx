open System.IO


let explode (s:string) : char list = [for c in s do yield c]

let readFile file  =
    File.ReadAllLines file
    |> List.ofArray
    |> List.map explode
    |> List.concat



let data = readFile "input.txt"

let rec getFloor acc = function
    | '('::xs -> getFloor (acc + 1) xs
    | ')'::xs -> getFloor (acc - 1) xs
    | _::xs -> getFloor acc xs
    | _ -> acc

getFloor 0 data
|> printfn "Solution part 1: %d"

// Part 2
let rec enterBasement acc lastPos xs =
    if acc < 0 then lastPos
    else
        match xs with
            | '('::xs -> enterBasement (acc + 1) (lastPos + 1) xs
            | ')'::xs -> enterBasement (acc - 1) (lastPos + 1) xs
            | _::xs ->   enterBasement acc (lastPos + 1) xs
            | _ -> 0

enterBasement 0 0 data
|> printfn "Solution part 2: %d"
