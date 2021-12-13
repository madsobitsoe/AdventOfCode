open System.IO
open System.Text.RegularExpressions


let explode (s:string) : char list = [for c in s do yield c]

let to2dArray (xs : (int * int) list) =
    let maxx = List.map fst xs |> List.max
    let maxy = List.map snd xs |> List.max
    Array2D.init (maxy+1) (maxx+1) (fun y x -> match (List.tryFind ((=) (x,y)) xs) with None -> '.' | Some _ -> '#')

let readFile file =
    File.ReadAllText file
    |> (fun (s:string) -> s.Split("\n\n"))
    |> Array.toList
    |> (fun ((x::y::_):string list) -> x,y)
    |> (fun ((a:string),b) -> a.Split("\n") |> Array.toList,b)
    |> (fun ((a:string list),b) -> List.map (fun (s:string) -> s.Split(",") |> Array.toList) a,b)
    |> (fun ((a:string list list),b) -> List.map (fun (x::y::_) -> x,y) a,b)
    |> (fun ((ss:(string * string) list),b) -> List.map (fun (x,y) -> (int x, int y)) ss,b) 
    |> (fun (a,b) -> to2dArray a,b)


let foldX x xs =
    let maxx = Array2D.length2 xs - 1
    let maxy = Array2D.length1 xs
    Array2D.init maxy x (fun i j -> if xs.[i,j] = '#' || xs.[i,maxx - j] = '#' then '#' else '.')

let foldY y xs =
    let maxx = Array2D.length2 xs 
    let maxy = Array2D.length1 xs - 1
    Array2D.init y maxx (fun i j -> if xs.[i,j] = '#' || xs.[maxy - i, j] = '#' then '#' else '.')


let test = readFile "test.txt"
let data = readFile "input.txt"


let countDots xs =
    let mutable c = 0L
    for y = 0 to Array2D.length1 xs - 1 do
        for x = 0 to Array2D.length2 xs - 1 do
            if xs.[y,x] = '#' then c <- c + 1L

    c
    
foldX 655 (fst data)
|> countDots
|> printfn "Solution part 1: %d"


// Part 2
type Fold = X of int | Y of int
let (|ParseRegex|_|) regex str =
   let m = Regex(regex).Match(str)
   if m.Success
   then Some (List.tail [ for x in m.Groups -> x.Value ])
   else None

let (|Entry|_|) = function
    | ParseRegex "fold along x=([0-9]+)" [x] -> Some (int x |> X)
    | ParseRegex "fold along y=([0-9]+)" [y] -> Some (int y |> Y)    
    | _ -> None

let parseInstruction = function
    | Entry e -> e
    | _ -> failwith "oops"
    
let instructions =
    snd data |> (fun (s : string) -> s.Split("\n")) |> Array.filter ((<>) "")
    |> Array.map  parseInstruction
    |> Array.toList

let p2 = List.fold (fun acc inst -> match inst with | X x -> foldX x acc | Y y -> foldY y acc) (fst data) (instructions )

printfn "Solution part 2:"
for y = 0 to Array2D.length1 p2 - 1 do
    for x = 0 to Array2D.length2 p2 - 1 do
        printf "%c" (p2.[y,x])
    printfn ""

