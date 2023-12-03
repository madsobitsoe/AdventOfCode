open System.IO
open System.Text.RegularExpressions

let explode (s:string) : char list = [for c in s do yield c]
let isDigit (s:string) = System.Text.RegularExpressions.Regex("[0-9]").IsMatch s

// Partial active pattern for parsing an int
let (|Integer|_|) (str: string) =
   let mutable intvalue = 0
   if System.Int32.TryParse(str, &intvalue) then Some(intvalue)
   else None

let (|ParseRegex|_|) regex str =
   let m = Regex(regex).Match(str)
   if m.Success
   then Some (List.tail [ for x in m.Groups -> x.Value ])
   else None

let (|CoordPair|_|) = function
    | ParseRegex "(\d+)-(\d+),(\d+)-(\d+)" [Integer x0;Integer y0;Integer x1;Integer y1] -> Some ((x0,y0),(x1,y1))
    | _ -> None

let getCoordPair = function
    | CoordPair cp -> cp
    | _ -> failwith "bad data"


let readFile file  =
    File.ReadAllLines file
    |> List.ofArray
    |> List.map getCoordPair
    
let data = readFile "input.txt"
//let data = readFile "test.txt"


let isContained ((p1s,p1e),(p2s,p2e)) =
    (p1s <= p2s && p1e >= p2e) || (p1s >= p2s && p1e <= p2e)


let isOverlapping ((p1s,p1e),(p2s,p2e)) =
    if p1s = p2s || p1e = p2e then true
    else if p1s < p2s then
        p1e >= p2s
    else
        p1s <= p2e

        
List.filter isContained data |> List.length
|> printfn "Part 1: %d"

List.filter isOverlapping data
|> List.length
|> printfn "Part 2: %d"

