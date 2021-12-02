// Contains a horrible solution (top) and very nice solution (bottom)
open System.IO
open System.Text.RegularExpressions

// Just reads the file into a list of strngs
let readFile file  =
    File.ReadAllLines file
    |> List.ofArray

// Load the input data and the test data
let data = readFile "input.txt"
let test = readFile "test.txt"

// --------------------------------
// Do not try this at home, oh dear lord this is bad
// --------------------------------
let rec part1_bad (hpos,d) (xs:string list)  =
    match xs with
        | x::xs ->
            match x.[0] with
                | 'f' ->
                    let n = x.[8] |> int
                    part1_bad (hpos + (n-48), d) xs
                | 'd' ->
                    let n = x.[5] |> int
                    part1_bad (hpos, d + (n-48)) xs
                | 'u' ->
                    let n = x.[3] |> int
                    part1_bad (hpos, d - (n-48)) xs
        | _ -> (hpos * d)

part1_bad (0,0) test |> printfn "Part 1 test-solution: %d"
part1_bad (0,0) data |> printfn "Part 1 solution: %d"

/// part 2 - very bad an very error prone
let rec part2_bad (hpos,d,aim) (xs:string list)  =
    match xs with
        | x::xs ->
            match x.[0] with
                | 'f' ->
                    let n = x.[8] |> int
                    part2_bad (hpos + (n-48), (d + aim * (n-48)), aim ) xs
                | 'd' ->
                    let n = x.[5] |> int
                    part2_bad (hpos, d, aim + (n-48)) xs
                | 'u' ->
                    let n = x.[3] |> int
                    part2_bad (hpos, d, aim - (n-48)) xs
        | _ -> (hpos * d)




// --------------------------------
// Active patterns for the win!
// Much better solution
// --------------------------------

// A type for instructions
type Instruction = Forward of int | Up of int | Down of int

// Partial active pattern for parsing an int
let (|Integer|_|) (str: string) =
   let mutable intvalue = 0
   if System.Int32.TryParse(str, &intvalue) then Some(intvalue)
   else None

// ParseRegex parses a regular expression and returns a list of the strings that match each group in
// the regular expression.
// List.tail is called to eliminate the first element in the list, which is the full matched expression,
// since only the matches for each group are wanted.
// Stolen straight from this example:
// https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/active-patterns
let (|ParseRegex|_|) regex str =
   let m = Regex(regex).Match(str)
   if m.Success
   then Some (List.tail [ for x in m.Groups -> x.Value ])
   else None

let (|Forward|_|) = function
    | ParseRegex "forward (\d+)" [Integer n] -> Forward n |> Some
    | _ -> None
let (|Up|_|) = function
    | ParseRegex "up (\d+)" [Integer n] -> Up n |> Some
    | _ -> None
let (|Down|_|) = function
    | ParseRegex "down (\d+)" [Integer n] -> Down n |> Some
    | _ -> None


let parseInstructionPart1 (hpos, depth) = function
    | Forward n -> (hpos + n, depth)
    | Up n -> (hpos, depth-n)
    | Down n -> (hpos, depth+n)
    | _ -> failwith "Unexpected instruction"

let parseInstructionPart2 (hpos, depth, aim) = function
    | Forward n -> (hpos + n, depth + n * aim, aim)
    | Up n -> (hpos, depth, aim - n)
    | Down n -> (hpos, depth, aim + n)
    | _ -> failwith "Unexpected instruction"


// Recurse through the list of input, parsing and applying instructions as we go
let rec part1 xs acc =
    match xs with
        | x::xs -> parseInstructionPart1 acc x |> part1 xs
        | _ -> let (hpos,d) = acc in hpos*d

// Recurse through the list of input, parsing and applying instructions as we go
let rec part2 xs acc =
    match xs with
        | x::xs -> parseInstructionPart2 acc x |> part2 xs
        | _ -> let (hpos,d,_) = acc in hpos*d

part1 data (0,0) |> printfn "Part 1 solution: %d"
part2 data (0,0,0) |> printfn "Part 2 solution: %d"
