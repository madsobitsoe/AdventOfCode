open System.IO

let readFile file =
    File.ReadAllLines file
    |> List.ofArray
    |> List.map List.ofSeq


let data = readFile "test.txt"
let data = readFile "input.txt"
