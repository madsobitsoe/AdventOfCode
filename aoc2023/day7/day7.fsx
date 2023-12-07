open System.IO

let explode (s:string) : char list = [for c in s do yield c]

let readFile file =
    File.ReadAllLines file
    |> Array.map (fun s -> s.Split " ")
    |> Array.map (fun x -> Array.head x, (Array.skip 1 >> Array.head) x |> int64)
    |> List.ofArray

    
// let data = readFile "test.txt"
let data = readFile "input.txt"

type Hand =
    | FiveOfAKind
    | FourOfAKind
    | FullHouse
    | ThreeOfAKind
    | TwoPairs
    | Pair
    | Highcard

let convert = function
    | '2' -> 2
    | '3' -> 3
    | '4' -> 4
    | '5' -> 5
    | '6' -> 6
    | '7' -> 7
    | '8' -> 8
    | '9' -> 9
    | 'T' -> 10
    | 'J' -> 11
    | 'Q' -> 12
    | 'K' -> 13
    | 'A' -> 14
    | _ -> failwith "card not recognized"
    
let convertJoker = function
    | 11 -> 1
    | x -> x

let convert2 = convert >> convertJoker

let replaceJoker (hand:int list) : int list list =
    let cards = [2;3;4;5;6;7;8;9;10;11;12;13;14]
    let replace y x = if x = 1 then y else x
    let replacers = List.map replace cards
    let newHands = List.map (fun x -> List.map x hand) replacers
    newHands

let rec getType hand =
    let getHighJokerHandType =
        replaceJoker
        >> List.map getType
        >> List.sort
        >> List.head 

    if List.contains 1 hand then
        getHighJokerHandType hand
    else
        let grouped = List.groupBy id hand
        match List.length grouped with
            | 1 -> FiveOfAKind
            | 2 -> // Can be four of a kind or full house
                if List.exists (fun (_,x) -> List.length x = 4) grouped
                then FourOfAKind
                else FullHouse
            | 3 -> // Can be ThreeOfAKind or two pairs
                if List.exists (fun (_,x) -> List.length x = 3) grouped
                then ThreeOfAKind
                else TwoPairs
            | 4 -> Pair
            | 5 -> Highcard
            | _ -> failwith "oopsie"

let solve converter data =
    data
    |> List.map (fun (h,b) -> (explode >> List.map converter) h,b)
    |> List.map (fun (a,b) -> getType a,a,b)
    |> List.groupBy (fun (t,_,_) -> t)
    |> List.sortByDescending fst
    |> List.map (fun (t',h') -> t',List.sortBy (fun (t,h,b) ->  h) h')
    |> List.collect snd
    |> List.mapi (fun i (_,_,b) -> (int64 (i+1)) * b)
    |> List.sum


solve convert data
|> printfn "Part 1: %A"

solve convert2 data
|> printfn "part 2: %A"

