open System.IO
open System.Text.RegularExpressions

// Just reads the file of binary numbers into a list of ints
let readFile file  =
    File.ReadAllLines file
    |> List.ofArray

// Load the input data and the test data
let data = readFile "input.txt"
let test = readFile "test.txt"

let getNumbers (data:string list) =
    match data with
        | x::xs -> x.Split(",") |> Array.toList |> List.map int
        | _ -> failwith "Ooops, no numbers"



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

let (|Row|_|) = function
    | ParseRegex "(\d+)[ ]+(\d+)[ ]+(\d+)[ ]+(\d+)[ ]+(\d+)" [Integer a; Integer b; Integer c; Integer d; Integer e] -> Some [a;b;c;d;e]
    | _ -> None


type bingonumber = int * bool    
type plate = bingonumber list list



let parseRow = function
    | Row row -> row
    | _ -> failwith "No row found"

let rec parsePlate i acc data =
    if i <= 0 then List.rev acc
    else
        match data with
            | x::xs -> parsePlate (i-1) ((parseRow x)::acc) xs
            | _ -> failwith "No plate found" 
    

let rec parsePlates data =
    match data with
        | [] -> []
        | ""::xs -> parsePlates xs
        | " "::xs -> parsePlates xs
        | data -> parsePlate 5 [] data :: parsePlates data.[5..]


let parseInput data =
    let numbers = getNumbers data
    let plates = parsePlates data.[2..] |> List.map (fun xss -> List.map (fun xs -> List.map (fun x -> x,false) xs) xss)
    (numbers, plates)
    
let checkPlate plate =
    // For each row or column, if all marked plate has won
    let rowCheck = List.map (List.forall (fun (_,b) -> b)) plate
    let colCheck = List.transpose plate |> List.map (List.forall (fun (_,b) -> b))
    List.contains true (rowCheck @ colCheck)

let getWinnerPlate plates = List.filter checkPlate plates |> List.head

let getUnmarked plate = List.concat plate |> List.filter (fun (_,b) -> not b) |> List.map (fun (n,_) -> n)

let markPlates plates n =
    List.map (fun plate ->
              List.map (fun row ->
                        List.map (fun (elm,b) -> if elm = n then (elm,true) else (elm,b)) row) plate) plates

let rec playBingo (numbers:int list) (plates : (int * bool) list list list) =
    match numbers with
        | [] -> failwith "No winner"
        | x::xs ->
            // Mark plates
            let plates' = markPlates plates x
            // Check if we have a winner
            if List.map checkPlate plates' |> List.contains true then
                let winPlate = getWinnerPlate plates'
                let unmarkedSum = getUnmarked winPlate |> List.sum
                x * unmarkedSum
            else playBingo xs plates'


parseInput data ||> playBingo |> printfn "Part 1 solution: %d"


// Part 2
let getLoserPlates plates = List.filter ( checkPlate >> not) plates 

let rec loseBingo (numbers:int list) (plates : (int * bool) list list list) =
    match numbers with
        | [] -> failwith "No winner"
        | x::xs ->
            // Mark plates
            let plates' = markPlates plates x
            if List.length plates' = 1 then
                // Check if we have won as the last player
                if checkPlate (plates'.[0]) then
                    printfn "x: %d\nplate: %A" x (plates'.[0])                    
                    getUnmarked (plates'.[0]) |> List.sum |> (*) x
                else
                    loseBingo xs plates'
            else
                // Filter out winners if any
                if List.map checkPlate plates' |> List.contains true then
                    let loserPlates = getLoserPlates plates'
                    loseBingo xs loserPlates
                else loseBingo xs plates'


parseInput data ||> loseBingo |> printfn "Part 2 solution: %d"
