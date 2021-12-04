open System.IO

// Just reads the file of binary numbers into a list of ints
let readFile file  =
    File.ReadAllLines file
    |> List.ofArray
    |> List.map (fun x -> System.Convert.ToInt32(x, 2))

// Load the input data and the test data
let data = readFile "input.txt"
// let test = readFile "test.txt"

// get most common bit in position i (0 is LSB)
let getBit bytes i len  =
    List.fold (fun acc x -> (x >>> i &&& 1) + acc) 0 bytes
    |> (fun x -> if x > len / 2 then 1u else 0u)


// part 1
// Oh wow this is not pretty
let getByte bytes =
    let len = List.length bytes
    let one = getBit bytes 0 len
    let two = getBit bytes 1 len
    let three = getBit bytes 2 len
    let four = getBit bytes 3 len
    let five = getBit bytes 4 len
    let six = getBit bytes 5 len
    let seven = getBit bytes 6 len
    let eight = getBit bytes 7 len
    let nine = getBit bytes 8 len
    let ten = getBit bytes 9 len
    let eleven = getBit bytes 10 len
    let twelve = getBit bytes 11 len

    let gamma = (one ||| (two <<< 1) ||| (three <<< 2) ||| (four <<< 3) ||| (five  <<< 4) ||| (six <<< 5) ||| (seven <<< 6) ||| (eight <<< 7) ||| (nine <<< 8) ||| (ten <<< 9) ||| (eleven <<< 10) ||| (twelve <<< 11))
    let eps = (gamma ^^^ (4096u - 1u))
    gamma,eps

getByte data ||> (*)
|> printfn "Part 1 solution: %d"


// Part 2
// Filter a list, to keep only the elements containing the least/most common bit i pos i
let keepCommon op bytes i bit = List.filter (fun x -> (op) ((x >>> i) &&& 1) bit) bytes
let keepMostCommon = keepCommon (=)
let keepLeastCommon = keepCommon (<>)

// Higher order oxygen{gen,scrub} function
let rec oxygen f i bytes =
    match bytes with
        | [x] -> x
        | _ ->
            let len = (List.length bytes) - 1
            let bit = getBit bytes i len
            oxygen f (i-1) (f bytes i (int bit))

let rec oxygenGen = oxygen keepMostCommon
let rec oxygenScrub = oxygen keepLeastCommon

// Part 2 solution
(oxygenGen 11 data, oxygenScrub 11 data) ||> (*)
|> printfn "Part 2 solution: %d"
