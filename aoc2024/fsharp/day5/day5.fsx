open System.IO
open System.Text.RegularExpressions

let explode (s:string) : string list = [for c in s do yield c] |> List.map string


let (|Integer|_|) (str: string) =
   let mutable intvalue = 0
   if System.Int32.TryParse(str, &intvalue) then Some(intvalue)
   else None


let (|ParseRegex|_|) regex str =
   let m = Regex(regex).Match(str)
   if m.Success
   then Some (List.tail [ for x in m.Groups -> x.Value ])
   else None

let (|Rule|_|) = function
    | ParseRegex "(\d+)\|(\d+)" [Integer a; Integer b] -> Some (a,b)
    | _ -> None

let getRules = function
    | Rule r -> r
    | x -> failwith <| sprintf "Bad data: %A" x


let getUpdates = function
    | Integer i -> i
    | x -> failwith <| sprintf "Bad data: %A" x

let addToMap map (k,v) =
    map |> Map.change k (fun x ->
                         match x with
                         | Some v' -> Some (v::v')
                         | None -> Some ([v])
                         )

let splitData (rules:string) (updates:string) =
    let rules' =
        rules.Split '\n'
        |> Array.map getRules
        |> Array.fold addToMap Map.empty

    let updates' =
        updates.Split '\n'
        |> Array.map (fun s -> s.Split ',')
        |> Array.filter ((<>) [|""|])
        |> Array.map (Array.map getUpdates)
        |> List.ofArray
        |> List.map List.ofArray
        
    rules',updates'


let readFile file =
    File.ReadAllText file
    |> (fun s -> s.Split "\n\n")
    |> (fun ss -> ss.[0],ss.[1])
    ||> splitData



//let (rules,updates) = readFile "test.txt"
let (rules,updates) = readFile "input.txt"


let rec isValidUpdate rules seen update =
    match update with
        | [] -> true
        | u::us ->
            let goesBefore =
                match Map.tryFind u rules with
                    | Some l -> l
                    | None -> []
            if List.exists (fun x -> List.contains x seen) goesBefore then false
            else isValidUpdate rules (u::seen) us


let validUpdates =
    List.filter (isValidUpdate rules []) updates 

let getMiddle xs =
    let middle = List.length xs / 2
    xs.[middle]


let invalidUpdates =
    List.filter (fun x -> not (isValidUpdate rules [] x)) updates 


// Part1
List.map getMiddle validUpdates
|> List.sum
|> printfn "Part 1: %d"


// Part 2

let sorter' (rules: Map<int, int list>) n m =
    let nGoesBefore =
        match Map.tryFind n rules with
            | Some l -> l
            | None -> []
    let mGoesBefore =
        match Map.tryFind m rules with
            | Some l -> l
            | None -> []

    match (List.contains m nGoesBefore, List.contains n mGoesBefore) with
        | (true,false) -> -1
        | (false,true) -> 1
        | (true,true) ->
            // printfn "Weird: &d, %d" n m
            0
        | _ -> 0

let sorter = sorter' rules

invalidUpdates
|> List.map (List.sortWith sorter)
|> List.map getMiddle
|> List.sum
|> printfn "Part 2: %d"
