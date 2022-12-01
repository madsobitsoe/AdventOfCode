open System.IO
open System.Text.RegularExpressions

let explode (s:string) : char list = [for c in s do yield c]

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
    | ParseRegex "(\d+),(\d+) -> (\d+),(\d+)" [Integer x0;Integer y0;Integer x1;Integer y1] -> Some ((x0,y0),(x1,y1))
    | _ -> None

let getCoordPair = function
    | CoordPair cp -> cp
    | _ -> failwith "bad data"


let onlyHorVer : (((int * int) * (int * int)) list) -> (((int * int) * (int * int)) list) = List.filter (fun ((x0,y0), (x1,y1)) -> x0 = x1 || y0 = y1)


let readFile file  =
    File.ReadAllLines file
    |> List.ofArray
    |> List.map getCoordPair
    
let data = readFile "input.txt"
let test = readFile "test.txt"

let computeLine (p1, p2) =
    // Is line horizontal or vertical?
    let x0,y0 = p1
    let x1,y1 = p2
    let vertical = x0 = x1
    let horizontal = y0 = y1
    if vertical then [for y in [min y0 y1..max y0 y1] do yield (x0,y)]
    elif horizontal then [for x in [min x0 x1..max x0 x1] do yield (x,y1)]
    // diagonal
    else
        let lx,ly = if x0 < x1 then p1 else p2
        let rx,ry = if x0 > x1 then p1 else p2
        let slope = if ly < ry then 1 else -1
        List.map2 (fun x y -> (x,y)) [lx..rx] [ly..slope..ry]

data |> onlyHorVer
|> List.map computeLine
|> List.concat
|> List.countBy id
|> List.filter (fun (_,c) -> c > 1)
|> List.length
|> printfn "Solution part 1: %d" 


// Stupid visualizer because my generated lines were messed up and I needed to see it
let drawLine y x0 x1 ps =
    let mutable s = ""
    for x in [x0..x1] do
        match List.tryFind (fun ((x',y'),_) -> x' = x && y' = y) ps with
            | Some (_,c) -> printf "%d" c
            | None -> printf "."
    printfn ""

let drawGrid ps =
    let height = List.sortByDescending (fun ((_,y),_) -> y) ps |> List.head |> (fun ((_,y),_) -> y)
    let width = List.sortByDescending (fun  ((x,_),_) -> x) ps |> List.head |> (fun ((x,_),_) -> x)
    for y in [0..height] do drawLine y 0 width ps
    
// test
// |> List.map computeLine
// |> List.concat
// |> List.sort
// |> List.countBy id
// |> List.sortBy (fun (_,c) -> c)
// |> drawGrid

// Part 2
data
|> List.map computeLine
|> List.concat
|> List.countBy id
|> List.filter (fun (_,c) -> c > 1)
|> List.length
|> printfn "Solution part 2: %d" 
