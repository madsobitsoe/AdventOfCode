open System.IO

let explode (s:string) : char list = [for c in s do yield c]

let readFile file =
    File.ReadAllLines file
    |> List.ofArray
    |> List.map explode

//let data = readFile "test.txt"
let data = readFile "input.txt"

// Somehow expand the galaxy
let rec expandRows acc galaxy =
    match galaxy with
        | [] -> List.rev acc
        | x::xs ->
            let acc' =
                if List.forall ((=) '.') x
                then x::x::acc
                else x::acc
            expandRows acc' xs

let expandColumns galaxy =
    List.transpose galaxy
    |> expandRows []
    |> List.transpose


let diff (a,b) (x,y) =
    abs (a-x), abs (b-y)

let diff64 (a:int64, b:int64) (x,y) =
    abs (a-x), abs(b-y)


let tsum (a,b) =
    a + b

let tsum64 (a:int64,b:int64) =
    a + b

let tadd (a:int64,b:int64) (x,y) =
    a+x,b+y


    
let rec pairs acc x xs =
    match xs with
        | [] -> acc
        | y::ys -> pairs ((x,y)::acc) x ys

let rec allPairs acc xs =
    match xs with
        | [] -> acc |> List.concat
        | x::xs ->
            let xPairs = pairs [] x xs
            allPairs (xPairs::acc) xs


allPairs [] [1..4]


expandRows [] data |> expandColumns
|> List.mapi (fun i x -> List.mapi (fun j y -> if y = '#' then Some (i,j) else None) x)
|> List.concat
|> List.filter ((<>) None)
|> List.map Option.get
|> allPairs []
|> List.map (fun (a,b) -> diff a b)
|> List.map tsum
|> List.sum
|> printfn "Part 1: %d"

// Part 2

let idata = List.mapi (fun i x -> List.mapi (fun j y -> (int64 i, int64 j),y) x) data
let rec expandRowsBy expander acc (galaxy:((int64 * int64) * char ) list list) =
    match galaxy with
        | [] -> List.rev acc
        | x::xs ->
            if List.forall (snd >> (=) '.') x then
                let xs' = List.map (List.map expander) xs
                expandRowsBy expander (x::acc) xs'
            else
                expandRowsBy expander (x::acc) xs

let rowExpander = (fun (a,b) -> tadd (999999L,0L) a, b)
let colExpander = (fun (a,b) -> tadd (0L,999999L) a, b)

expandRowsBy rowExpander [] idata
|> List.transpose
|> expandRowsBy colExpander []
|> List.transpose
|> List.concat
|> List.map (fun (c,x) -> if x = '#' then Some c else None)
|> List.filter ((<>) None)
|> List.map Option.get
|> allPairs []
|> List.map (fun (a,b) -> diff64 a b)
|> List.map tsum64
|> List.sum
|> printfn "Part 2: %A"
