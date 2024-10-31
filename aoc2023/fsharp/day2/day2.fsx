open System.IO
open System.Text.RegularExpressions


type Color = Red | Green | Blue
type Draw = Color * int
type Game = int * Draw list list


let explode (s:string) : string list = [for c in s do yield c] |> List.map string

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

let (|Index|_|) = function
    | ParseRegex "Game (\d+)" [Integer g] -> Some g
    | _ -> None

let getGameIndices = function
    | Index idx -> idx
    | x -> failwith <| sprintf "bad data: %s" x

let (|Color|_|) = function
    | ParseRegex "(blue)" [s] -> Some Blue
    | ParseRegex "(red)" [s] -> Some Red
    | ParseRegex "(green)" [s] -> Some Green
    | _ -> None

let (|Draw|_|) = function
    | ParseRegex "(\d+) (blue|red|green)" [Integer n; Color s] -> Some (s,n)
    | _ -> None


let getDraws = function
    | Draw d -> d
    | x -> failwith <| sprintf "bad data: %s" x

// Parsing is hard and boring
let readFile file  =
    File.ReadAllLines file
    |> Array.map (fun (s:string) -> s.Split ": ")
    |> List.ofArray
    |> List.map List.ofArray
    |> List.map (fun x -> x.[0],x.[1])
    |> List.map (fun x -> fst x |> getGameIndices, snd x)
    |> List.map (fun (g,gs) -> g, gs.Split ";" |> List.ofArray)
    |> List.map (fun (g,gs) -> g, List.map (fun (x:string) -> x.Split ", " |> List.ofArray) gs)
    |> List.map (fun (g,gs) -> g, List.map (List.map getDraws) gs)
    |> List.map (fun (g,gs) -> g, List.concat gs)




let data = readFile "input.txt"
// let data = readFile "test.txt"

let part1 data =
    let red = 12
    let green = 13
    let blue = 14
    let filter = function
        | (Red, n) -> n <= red
        | (Green, n) -> n <= green
        | (Blue,n) -> n <= blue

    let filtered = List.filter (fun (g,gs) -> List.forall filter gs) data
    let indices : int list = List.map fst filtered
    List.sum indices




part1 data
|> printfn "Part 1: %d"

let part2Inner data =
    let max = List.map snd >> List.max
    // Find the max amount of each color in each game
    let maxRed = List.filter (fun x -> match x with | (Red,_) -> true | _ -> false) data |> max
    let maxBlue = List.filter (fun x -> match x with | (Blue,_) -> true | _ -> false) data |> max
    let maxGreen = List.filter (fun x -> match x with | (Green,_) -> true | _ -> false) data |> max
    maxRed * maxBlue * maxGreen


let part2 =
    List.map (snd >> part2Inner)
    >> List.sum


part2 data
|> printfn "Part 2: %d"
