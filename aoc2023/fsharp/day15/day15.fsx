open System.IO

let explode (s:string) : char list = [for c in s do yield c]

let readFile file =
    File.ReadAllText file
    |> (fun s -> s.Split ",")
    |> Array.map explode
    |> List.ofArray
    |> List.map (List.filter ((<>) '\n'))

//let data = readFile "test.txt"
let data = readFile "input.txt"

List.map (List.map int) data
|> List.map (fun x -> List.fold (fun acc x -> ((acc + x) * 17) % 256) 0 x)
|> List.sum
|> printfn "Part 1: %d"


// Part 2
let hash (xs:char list) =
    List.takeWhile (fun x -> x <> '-' && x <> '=') xs
    |> List.map int
    |> List.fold (fun acc x -> ((acc + x) * 17) % 256) 0

type Op =
    | Add of string * int
    | Remove of string

let parse xs =
    let label = List.takeWhile (fun x -> x <> '-' && x <> '=') xs |> List.map string |> List.reduce (+)
    let op = List.skipWhile (fun x -> x <> '-' && x <> '=') xs |> List.head 
    match op with
        | '=' -> Add (label, (List.last xs |> int |> (fun x -> x - 48)) )
        | '-' -> Remove label
        | x -> failwith <| sprintf "parse: %A :: %A\nlabel: %s\nop: %A" xs x label op

let rec insert' elm xs =
    let l',s' = elm
    match xs with
        | [] -> [elm]
        | (l,s)::xs when l = l' -> (l',s')::xs
        | x::xs -> x::insert' elm xs

let rec remove' l' xs =
    match xs with
        | [] -> []
        | (l,s)::xs when l = l' -> xs
        | x::xs -> x:: remove' l' xs

let doOp op map =
    match op with
        | Add (label,v) ->
            let idx = explode label |> hash
            let xs = Map.find idx map
            let xs' = insert' (label,v) xs
            let map' = Map.add idx xs' map
            map'
        | Remove label ->
            let idx = explode label |> hash
            let xs = Map.find idx map
            let xs' = remove' label xs
            let map' = Map.add idx xs' map
            map'


let map : Map<int,(string*int) list> = [0..255] |> List.map (fun x -> x,[]) |> Map

List.map parse data
|> List.fold (fun acc x -> doOp x acc) map
|> Map.toList
|> List.map (fun (n,l) -> n+1,l)
|> List.collect (fun (idx,b) -> List.mapi (fun i x -> (i+1) * snd x * idx) b)
|> List.sum
|> printfn "Part 2: %d"
