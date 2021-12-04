open System.IO

// Just reads the file of binary numbers into a list of ints
let readFile file  =
    File.ReadAllLines file
    |> List.ofArray
    |> List.map int

// Load the input data and the test data
let data = readFile "input.txt"
let test = readFile "test.txt"



// Find two entries that sum to 2020
[for x in data do for y in data do if x + y = 2020 then yield x * y]
|> List.head
|> printfn "Solution part 1: %d"

// Find three entries that sum to 2020
[for x in data do for y in data do for z in data do if x + y + z = 2020 then yield x * y * z]
|> List.head
|> printfn "Solution part 2: %d"



