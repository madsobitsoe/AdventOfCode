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
    

let getType card =
    // let l = explode card
    let l = card
    let grouped = List.groupBy id l
    match List.length grouped with
        | 1 -> FiveOfAKind
        | 2 ->
            // Can be four of a kind or full house
            if List.exists (fun (_,x) -> List.length x = 4) grouped
            then FourOfAKind
            else FullHouse
        | 3 ->
            // Can be ThreeOfAKind or two pairs
            if List.exists (fun (_,x) -> List.length x = 3) grouped
            then ThreeOfAKind
            else TwoPairs
        | 4 -> Pair
        | 5 -> Highcard
        | _ -> failwith "oopsie"

data
|> List.map (fun (h,b) -> (explode >> List.map convert) h,b)
|> List.map (fun (a,b) -> getType a,a,b)
|> List.groupBy (fun (t,h,b) -> t)
|> List.sortByDescending (fun (t,_) -> t)
|> List.map (fun (t',h') -> t',List.sortBy (fun (t,h,b) ->  h) h')
|> List.map (fun (_,hs) -> hs)
|> List.concat
|> List.mapi (fun i x -> i+1,x)
|> List.map (fun (r,(_,_,b)) -> (int64 r) * b)
|> List.sum
|> printfn "part 1: %A"


// Part 2
let convert2 = function
    | '2' -> 2
    | '3' -> 3
    | '4' -> 4
    | '5' -> 5
    | '6' -> 6
    | '7' -> 7
    | '8' -> 8
    | '9' -> 9
    | 'T' -> 10
    | 'J' -> 1
    | 'Q' -> 12
    | 'K' -> 13
    | 'A' -> 14
    | _ -> failwith "card not recognized"


let replaceJoker (hand:int list) : int list list =
    let cards = [2;3;4;5;6;7;8;9;10;11;12;13;14]
    let replace y x = if x = 1 then y else x
    let replacers = List.map replace cards
    let newHands = List.map (fun x -> List.map x hand) replacers
    newHands


let rec getType2 l =
    if List.contains 1 l then
        replaceJoker l
        |> List.map getType2
        |> List.sort
        |> List.head
    else
        let grouped = List.groupBy id l
        match List.length grouped with
            | 1 -> FiveOfAKind
            | 2 ->
                // Can be four of a kind or full house
                if List.exists (fun (_,x) -> List.length x = 4) grouped
                then FourOfAKind
                else FullHouse
            | 3 ->
                // Can be ThreeOfAKind or two pairs
                if List.exists (fun (_,x) -> List.length x = 3) grouped
                then ThreeOfAKind
                else TwoPairs
            | 4 -> Pair
            | 5 -> Highcard
            | _ -> failwith "oopsie"

data
|> List.map (fun (h,b) -> (explode >> List.map convert2) h,b)
|> List.map (fun (a,b) -> getType2 a,a,b)
|> List.groupBy (fun (t,h,b) -> t)
|> List.sortByDescending (fun (t,_) -> t)
|> List.map (fun (t',h') -> t',List.sortBy (fun (t,h,b) ->  h) h')
|> List.map (fun (_,hs) -> hs)
|> List.concat
|> List.mapi (fun i x -> i+1,x)
|> List.map (fun (r,(_,_,b)) -> (int64 r) * b)
|> List.sum
|> printfn "part 2: %A"


