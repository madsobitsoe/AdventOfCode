#time
open System.IO

let explode (s:string) = [for c in s do yield c]

let readFile file =
    File.ReadAllLines file
    |> List.ofArray
    |> List.map explode
    |> List.filter ((<>) [])

let addToMap map x =
    let k,v = x
    match Map.tryFind k map with
        | None -> Map.add k [v] map
        | Some vs -> Map.add k (v::vs) map

let toMap data =
    List.mapi (fun i x -> List.mapi (fun j y -> if y <> '.' then Some (y, (i,j)) else None) x) data
    |> List.concat 
    |> List.filter ((<>) None)
    |> List.map Option.get
    |> List.fold (fun map x -> addToMap map x) Map.empty 

//let data = readFile "test.txt"
let data = readFile "input.txt"
let dataMap = toMap data

let maxx =
    List.head data
    |> List.length
    |> (fun x -> x - 1)
let maxy =
    List.length data
    |> (fun x -> x - 1)

let inBounds' maxx maxy (i, j) =
    i >= 0 && i <= maxx && j >= 0 && j <= maxy

let inBounds = inBounds' maxx maxy

let placeAntinodes a b antinodes =
    let (ax,ay) = a
    let (bx,by) = b
    let (diffx,diffy) = (bx - ax), (by - ay)
    let a1 = (ax-diffx, ay - diffy)
    let a2 = (bx+diffx, by + diffy)
    let antinodes' = if inBounds a1 then Set.add a1 antinodes else antinodes
    if inBounds a2 then Set.add a2 antinodes' else antinodes'


let rec place antennas antinodes =
    match antennas with
        | [] | [_] -> antinodes
        | x::y::xs ->
            let antinodes' = placeAntinodes x y antinodes
            let antinodes'' = place (x::xs) antinodes'
            place (y::xs) antinodes''
            

let part1 dataMap =
    Map.fold (fun state k v -> place v state) (Set.empty) dataMap

part1 dataMap
|> Set.count
|> printfn "Part 1: %A"     


let tupAdd (a,b) (c,d) = a+c,b+d
let tupSub (a,b) (c,d) = c-a,d-b

let unfold c x =
    if inBounds x then Some (x, c x)
    else None

let placeAntinodes2 pos1 pos2 =
    let diff = tupSub pos1 pos2
    List.unfold (unfold (tupAdd diff)) pos1
    @ List.unfold (unfold (tupSub diff)) pos2

let rec place2 antennas antinodes =
    match antennas with
        | [] | [_] -> antinodes
        | x::y::xs ->
            let antinodes' = placeAntinodes2 x y |> List.fold (fun acc x -> Set.add x acc) antinodes
            let antinodes'' = place2 (x::xs) antinodes'
            place2 (y::xs) antinodes''

let part2 dataMap =
    Map.fold (fun state k v -> place2 v state) Set.empty dataMap

part2 dataMap
|> Set.count
|> printfn "Part 2: %d"
