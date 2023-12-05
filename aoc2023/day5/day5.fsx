open System.IO
open System.Text.RegularExpressions

let explode (s:string) : char list = [for c in s do yield c]
let isDigit (s:string) = Regex("[0-9]").IsMatch s
let isDigitOrLetter (s:string) = Regex("[0-9a-zA-Z]").IsMatch s



// Partial active pattern for parsing an int
let (|Integer|_|) (str: string) =
   let mutable intvalue = 0
   if System.Int32.TryParse(str, &intvalue) then Some(intvalue)
   else None

let (|Long|_|) (str: string) =
    let mutable longValue = 0L
    if System.Int64.TryParse(str, &longValue) then Some longValue
    else None

let (|ParseRegex|_|) regex str =
   let m = Regex(regex).Match(str)
   if m.Success
   then Some (List.tail [ for x in m.Groups -> x.Value ])
   else None


let (|Mapping|_|) = function
    | ParseRegex "(\d+) (\d+) (\d+)" [Long destination; Long source; Long size] -> Some (destination, source, size)
    | _ -> None



let getMapping = function
    | Mapping m -> m
    | _ -> failwith "bad data"


let rec translate map (d:int64)  =
    match map with
        | [] -> d
        | (dest,src,range)::xs ->
            if src <= d && d <= src+range then dest + (d-src)
            else translate xs d 


let readFile file =
    File.ReadAllText file
    |> (fun s -> s.Split "\n\n")
    |> Array.map (fun s -> s.Split "\n")
    |> Array.map List.ofArray
    |> List.ofArray
    |> List.map (List.filter ((<>) ""))

//let data = readFile "test.txt"
let data = readFile "input.txt"


let seeds =
    List.head data |> List.head
    |> (fun s -> s.Split " ")
    |> Array.tail
    |> Array.map int64
    |> List.ofArray


let genMap l =
    List.tail l
    |> List.map getMapping

let genMaps data =
    List.tail data
    |> List.map genMap

let maps = genMaps data |> List.map translate

let finalMap = List.reduce (>>) maps

List.map finalMap seeds
|> List.min
|> printfn "Part 1: %d"


let rec listToTuples xs : ('a * 'a) list =
    match xs with
        | [] -> []
        | x::y::xs -> (x,y):: listToTuples xs
        | _ -> failwith "oh heavens no"

let brute (start:int64,stop:int64) =
    let rec inner n stop (acc:int64) =
        if n >= stop then acc
        else
            inner (n+1L) stop <| min (finalMap n) acc
    inner start (start+stop) System.Int64.MaxValue

// Part 2, seeds are now ranges goddamnit
let seedsAsRanges =
    List.head data |> List.head
    |> (fun s -> s.Split " ")
    |> Array.tail
    |> Array.map int64
    |> List.ofArray
    |> listToTuples
    |> List.map (fun x -> brute x)


List.min seedsAsRanges
|> printfn "Part 2: %A"
