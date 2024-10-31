open System.IO
open System.Text.RegularExpressions

let explode (s:string) : char list = [for c in s do yield c]
let isDigit (s:string) = System.Text.RegularExpressions.Regex("[0-9]").IsMatch s
let isNumber (s:string) = System.Text.RegularExpressions.Regex("[0-9]+").IsMatch s


let readFile file =
    File.ReadAllLines file
    |> Array.map (fun s -> s.Split [|':';'|'|])
    |> Array.map (Array.map (fun s -> s.Split(" ")))
    |> Array.map (Array.map (Array.filter isNumber))
    |> List.ofArray
    |> List.map List.ofArray
    |> List.map (List.map List.ofArray)

//let data = readFile "test.txt"
let data = readFile "input.txt"

let getIndex xs  = (List.map List.head) xs
let getCoupons xs = (List.map List.tail) xs

let coupons = getCoupons data

let score coupon =
    let winningNumbers = List.head coupon
    let coupon = (List.skip 1 >> List.head) coupon
    let winners = List.filter (fun x -> List.contains x winningNumbers) coupon
    let scored = List.mapi (fun i x -> if i = 0 then 1 else 2) winners
    match scored with
        | [] -> 0
        | _ -> List.reduce (*) scored

List.map score coupons
|> List.sum
|> printfn "Part 1: %d"


// Part 2
let part2DataF (data:string list list) =
    let idx = (List.head >> List.head) data |> int
    let winningNumbers = (List.skip 1 >> List.head) data
    let coupon = (List.skip 2 >> List.head) data
    (idx,1),winningNumbers,coupon

let part2Data = List.map part2DataF data


let score2 ((idx,count),winningNumbers,coupon) =
    let winners = List.filter (fun x -> List.contains x winningNumbers) coupon
    List.mapi (fun i x -> idx + i + 1, count) winners


// I can't be arsed to look up how to use dictionaries so we'll do slow ass recursion for each update, yay!
let rec updateGame games update =
    match games with
        | [] -> []
        | x::xs ->
            let ui,uc = update
            let (idx,count),w,c = x
            if ui = idx then
                ((idx,uc+count),w,c)::xs
            else
                x :: updateGame xs update
            
let rec allGames acc games =
    match games with
        | [] -> acc
        | x::xs ->
            let (idx,count),w,c = x
            let multipliers' = score2 x
            // oh yeah, fold it baby
            let games' = List.fold updateGame xs multipliers' 
            allGames (acc+count) games'

allGames 0 part2Data
|> printfn "Part 2: %d"
    
