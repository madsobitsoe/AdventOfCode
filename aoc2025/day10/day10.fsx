open System.IO
open System.Text.RegularExpressions

let (|ParseRegex|_|) regex str =
   let m = Regex(regex).Match(str)
   if m.Success
   then Some (List.tail [ for x in m.Groups -> x.Value ])
   else None

type IndicatorLights = bool list
type Buttons = int list list
type JoltageRequirements = int list

type ManualLine =
    {
        IndicatorLights: IndicatorLights;
        Buttons: Buttons;
        JoltageRequirements : JoltageRequirements
    }

let createManualLine (il : IndicatorLights) (buttons : Buttons) (jr:JoltageRequirements) : ManualLine =
    {
        IndicatorLights = il;
        Buttons = buttons;
        JoltageRequirements = jr;
    }

let mapCharsToBools = function
    | '.' -> false
    | '#' -> true
    | c -> failwith <| sprintf "Unexpected char: %c" c

let (|BoolsFromChars|_|) str : (IndicatorLights option)=
    Seq.map mapCharsToBools str |> Seq.toList |> Some

let mapButtonsToIntList (str:string) =
    str.Replace("(", "").Replace(")","").Split(',')
    |> Array.map int
    |> List.ofArray

let (|Buttons|_|) (str:string) : (Buttons option)=
    str.Split(" ")
    |> Array.filter ((<>) "")
    |> Array.map mapButtonsToIntList
    |> List.ofArray |> Some

let (|JoltageRequirements|_|) (str:string) : (JoltageRequirements option) =
    str.Split(',')
    |> Array.filter ((<>) "")
    |> Array.map int
    |> List.ofArray
    |> Some

let (|ManualLine|_|) : (string -> ManualLine option) = function
    | ParseRegex "\[([.#]+)\]\s+((?:\((?:\d+,?)+\)\s+)+)\s*\{((?:\d+,?)+)\}" [BoolsFromChars bs; Buttons buttons; JoltageRequirements jr] -> createManualLine bs buttons jr |> Some
    | _ -> None

let getManualLine = function
    | ManualLine ml -> ml
    | x -> failwith <| sprintf "Unexpected manual line: %s" x

let readFile : (string -> ManualLine list) =
    File.ReadAllLines >> Array.map getManualLine >> List.ofArray

let extendButtons buttons =
    Seq.map (fun b -> 1L,b) buttons

let combineButtons ((b1c,b1):(int64*int list)) ((b2c,b2):(int64*int list)) =
    let c = b1c + b2c
    let buttons =
        b1 @ b2
        |> Seq.countBy id
        |> Seq.filter (fun (_,c) -> c % 2 <> 0)
        |> Seq.map fst
        |> Seq.sort
        |> Seq.distinct
        |> Seq.toList
    c,buttons


let combineAllButtons buttons buttons' =
    Seq.allPairs buttons buttons'
    |> Seq.map (fun (a,b) -> combineButtons a b)
    |> Seq.map (fun (c,x) -> c,List.sort x)
    |> Seq.sortBy fst
    |> Seq.distinct
    |> Seq.filter (snd >> List.isEmpty >> not)


let expectedToButtonsList expected =
    expected
    |> List.mapi (fun i x -> i,x)
    |> List.filter (snd)
    |> List.map fst

let leastPresses (ml: ManualLine) =
    let expected = ml.IndicatorLights |> expectedToButtonsList
    let buttons = extendButtons ml.Buttons
    let combiner = combineAllButtons buttons
    let rec inner buttons' =
        match Seq.tryFind (fun (_,b) -> b = expected) buttons' with
            | Some (c,_) -> c
            | None ->
                let nextButtons = combiner buttons'
                inner nextButtons
    inner buttons


//let data = readFile "test.txt"


let data = readFile "input.txt"

let folder (i,acc) x =
    printfn "i: %d" i
    ((i+1),(acc + leastPresses x))

data
|> List.fold folder (1,0L)
|> snd
|> printfn "Part 1: %d"


// Part fucking 2



// let extendButtons2 l buttons =
//     let zeroes = [for _ in 1 .. l do yield 0]
//     List.map (fun x -> List.mapi (fun i _ -> if List.contains i x then 1 else 0) zeroes) buttons
    



// let rec doStuff cache expected buttons best c current xs =
//     if best <> 0L && best < c then cache,Some best
//     else
//     if Set.contains current cache then
//         printfn "Exiting by cache"
//         cache,None
//     else

//     match xs with
//         | [] -> Set.add current cache,None
//         | x::xs ->
//             let next = List.map2 (+) current x
//             if next = expected then
//                 printfn "WHAAAAAAAT"
//                 cache,Some (c+1L)
//             else

//                 let combined = List.map2 (fun a b -> a,b) expected next
//                 printfn "combined: %A" combined

//                 if List.exists (fun (a,b) -> a < b) combined then
//                     let cache' = Set.add next cache
//                     doStuff cache' expected buttons best c current xs
                                        
//                 else
//                     match doStuff cache expected buttons best (c+1L) next buttons with
//                         | cache,Some cand ->
//                             let newBest = if best = 0L then cand else min cand best
//                             doStuff cache expected buttons newBest (c) current xs
// //                            doStuff cache expected buttons best c current xs
//                         | cache,None ->
//                             doStuff cache expected buttons best c current xs
                            
//                             // let newBest = if best = 0L then cand else min cand best
//                             // doStuff cache expected buttons newBest c current xs

// let leastPresses2 (ml: ManualLine) =
//     let expected = ml.JoltageRequirements
//     let buttons = extendButtons2 (Seq.length expected) ml.Buttons
//     let initial = [for _ in 1 .. List.length expected do yield 0]
//     let rec inner current buttons' =
//         match doStuff Set.empty expected buttons 0L 0L current buttons with
//             | cache,Some c -> c
//             | cache,None -> failwith "no answer :("
//     inner initial buttons



// let h = List.head data
// leastPresses2 h

// let bs = extendButtons2 (h.JoltageRequirements |> List.length) h.Buttons


// combineAllButtons2 bs bs
// |> List.map (fun (c,x) -> c,List.zip [5;4;3;2] x)
// |> List.filter (snd >> (List.forall (fun (a,b) -> a >= b)))


// leastPresses2 h


// let folder2 (i,acc) x =
//     printfn "i: %d" i
//     ((i+1),(acc + leastPresses2 x))


// List.head data

// List.head data
// |> leastPresses2

// data
// |> List.fold folder2 (1,0L)
// |> snd
// |> printfn "Part 2: %d"



// // extendButtons h.Buttons
// // h.JoltageRequirements

// // let eb = extendButtons h.Buttons
// // combineAllButtons2 eb eb
