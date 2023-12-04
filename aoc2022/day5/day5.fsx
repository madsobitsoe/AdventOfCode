open System.IO
open System.Text.RegularExpressions

let explode (s:string) : char list = [for c in s do yield c]
let isDigit (s:string) = Regex("[0-9]").IsMatch s
let isDigitOrLetter (s:string) = Regex("[0-9a-zA-Z]").IsMatch s

let readFile file =
    File.ReadAllLines file
    |> List.ofArray

//let data = readFile "test.txt"
let data = readFile "input.txt"

let splitData data =
    let rec findSplit data =
        match data with
            | ""::xs -> 0
            | x::xs -> 1 + findSplit xs
            | [] -> failwith "oopsie whoopsie"
    let split = findSplit data
    List.take split data, List.skip (split + 1) data
    
let stackData,instructionsData = splitData data


let stackBuilder token (acc: string list list) =
    let current::rest = acc
    if isDigit token then
        [] :: acc
    else
        (token :: current) :: rest
        

let buildStacks stackData =
    List.rev stackData
    |> List.map explode
    |> List.transpose
    |> List.concat
    |> List.map string
    |> List.filter isDigitOrLetter
    |> List.foldBack stackBuilder <| [[]]
    |> List.tail
    |> List.mapi (fun i x -> i+1,List.rev x)


let startStacks = buildStacks stackData


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

let (|Instruction|_|) = function
    | ParseRegex "move (\d+) from (\d+) to (\d+)" [Integer count; Integer source; Integer destination] -> Some (count, source, destination)
    | _ -> None

let getInstructions = function
    | Instruction i -> i
    | _ -> failwith "bad data"


let instructions = List.map getInstructions instructionsData

let getStack idx stacks = List.filter (fun (i,_) -> i = idx) stacks |> List.head |> snd

let rec setStack idx stack stacks =
    match stacks with
        | (i,x)::xs when i = idx -> stack::xs
        | x::xs -> x :: setStack idx stack xs
        | [] -> failwith "Oopsie whoopsie"
            
let takeFromStack idx count stacks =
    let stack = getStack idx stacks
    let taken = List.take count stack
    let rest = idx, List.skip count stack
    let newStacks = setStack idx rest stacks
    taken,newStacks

let putOnStack idx stack stacks =
    let currentStack = getStack idx stacks
    let newStack = List.rev stack @ currentStack
    setStack idx (idx,newStack) stacks


let execute stacks instruction =
    let count,source,destination = instruction

    let taken,stacks' = takeFromStack source count stacks
    putOnStack destination taken stacks'


let putOnStack2 idx stack stacks =
    let currentStack = getStack idx stacks
    let newStack = stack @ currentStack
    setStack idx (idx,newStack) stacks


let execute2 stacks instruction =
    let count,source,destination = instruction

    let taken,stacks' = takeFromStack source count stacks
    putOnStack2 destination taken stacks'




// What crate ends up on top of each crate? 
List.fold execute startStacks instructions
|> List.map (fun (_,x) -> List.head x)
|> List.reduce (+)
|> printfn "Part 1: %s"

// What crate ends up on top of each crate? 
List.fold execute2 startStacks instructions
|> List.map (fun (_,x) -> List.head x)
|> List.reduce (+)
|> printfn "Part 2: %s"
