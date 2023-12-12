open System.IO

let explode (s:string) : char list = [for c in s do yield c]

let readFile file =
    File.ReadAllLines file
    |> Array.map (fun s -> s.Split " ")
    |> Array.map (fun x -> Array.head x, (Array.tail >> Array.head) x)
    |> Array.map (fun (a,b) -> explode a, b.Split "," |> List.ofArray |> List.map int)
    |> List.ofArray


let data = readFile "test.txt"
let data = readFile "input.txt"

let rec replace (s : char list) =
    match s with
        | [] -> [[]]
        | '?'::xs ->
            let a = List.map (fun x -> '#' :: x) <| replace xs
            let b = List.map (fun x -> '.' :: x) <| replace xs
            a @ b
        | c::xs -> List.map (fun x -> c :: x) <| replace xs



let rec checkKey key input =
    if List.length input < key then None
    else
        match input with
            | [] -> None
            | '.'::xs -> checkKey key xs
            | _ ->
                let i' = List.take key input
                let checksOut = List.forall ((=) '#') i'
                let sanity =
                    if List.length input = key then
                         checksOut 
                    else
                        let next = List.skip key input |> List.head
                        checksOut && (next <> '#')
                if sanity && checksOut then Some (List.skip key input)
                else None
                
        

let rec isArrangement keys input =
    match keys with
        | [] -> List.forall ((<>) '#') input
        | x::xs ->
            match checkKey x input with
                | None -> false
                | Some input' -> isArrangement xs input'

let findArrangements (inputs,keys) =
    List.filter (isArrangement keys) inputs
    |> List.length
    
List.map (fun (a,b) -> replace a, b) data
|> List.map findArrangements
|> List.sum
|> printfn "Part 1: %d"


// Part 2

let rec joinWithQuestion xs =
    match xs with
        | [] -> []
        | x::[] -> x
        | x::xs -> x @ '?':: joinWithQuestion xs


let fixData (a,b) =
    (List.replicate 5 >> joinWithQuestion) a, (List.replicate 5 >> List.concat) b


// List.map fixData data
// |> List.map (fun (a,b) -> replace a, b) 
// |> List.map findArrangements
// |> List.map int64
// |> List.sum
// |> printfn "Part 2: %A"

