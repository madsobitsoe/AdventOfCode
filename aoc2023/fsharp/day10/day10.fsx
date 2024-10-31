open System.IO

let explode (s:string) : char list = [for c in s do yield c]

type Direction =
    | NorthSouth
    | WestEast
    | NorthEast
    | NorthWest
    | SouthWest
    | SouthEast
    | Start

let charToDirection = function
    | '|' -> Some NorthSouth
    | '-' -> Some WestEast
    | 'L' -> Some NorthEast
    | 'J' -> Some NorthWest
    | '7' -> Some SouthWest
    | 'F' -> Some SouthEast
    | 'S' -> Some Start
    | _ -> None


let neighbors = function
    | (i,j), NorthSouth -> [i-1,j;i+1,j]
    | (i,j), WestEast -> [i,j-1; i,j+1]
    | (i,j), NorthEast -> [i-1,j; i,j+1]
    | (i,j), NorthWest -> [i-1,j; i, j-1]
    | (i,j), SouthWest -> [i,j-1; i+1, j]
    | (i,j), SouthEast -> [i+1,j; i,j+1]
    | (i,j), Start -> [i-1,j; i+1,j; i,j-1; i,j+1]


let findStart map =
    Map.pick (fun c v -> if fst v = Start then Some c else None) map


let fixStart map =
    let startCoord = findStart map
    let (_,n) = Map.find startCoord map
    List.map (fun x -> x,Map.find x map) n
    |> List.filter (fun (_,(_,x)) -> List.contains startCoord x)
    |> List.map fst
    |> (fun x -> Map.add startCoord (Start,x) map)


let readFile file =
    File.ReadAllLines file
    |> List.ofArray
    |> List.map explode
    |> List.mapi (fun i x -> List.mapi (fun j v -> (i,j),v) x)
    |> List.concat
    |> List.map (fun (c,v) -> c, charToDirection v)
    |> List.filter (snd >> Option.isSome)
    |> List.map (fun (c,v) -> c, Option.get v)
    |> List.map (fun (c,v) -> c,(v, neighbors (c,v)))
    |> Map
    |> (fun m -> Map.map (fun k v -> fst v,List.filter (fun x -> Map.containsKey x m) (snd v)) m)
    |> fixStart

let data = readFile "input.txt"

let step' map from =
    let (_,next') = Map.find from map
    let next = List.filter ((<>) from) next'
    next

let step = step' data


let bfsVisit visited (next,count) =
    Map.add next count visited
    
let rec bfs (map:Map<(int*int),(Direction * (int*int) list)>)  (queue:((int*int)*int) list) visited =
    match queue with
        | [] -> visited
        | (x,count)::xs ->
            if Map.containsKey x visited then
                bfs map xs visited
            else
                let visited' = bfsVisit visited (x,count)
                let next =
                    step x
                    |> List.map (fun x -> x,count+1)
                bfs map (xs@next) visited'

let findLoop data =
    let start = findStart data
    bfs data [(start,0)] Map.empty


    
let solve loop = 
    loop
    |> Map.toList
    |> List.map snd
    |> List.max


let start = findStart data
let start' = Map.find start data |> snd
let loop = findLoop data


solve loop
|> printfn "Part 1: %d"


// Part 2
let readFile2 file =
    File.ReadAllLines file
    |> List.ofArray
    |> List.map explode


let data2 = readFile2 "input.txt"


let fixStart' (data:char list list) start start' : char list list =
    let f (i,j) (x,y) (z,w) =
        match (i-x,j-y),(i-z,j-w) with
            | (0,1),(0,-1) -> '|'
            | (0,-1),(0,1) -> '|'
            | (-1,0),(1,0) -> '-'
            | (1,0),(-1,0) -> '-'
            | (0,-1),(1,0) -> 'L'
            | (1,0),(0,-1) -> 'L'
            | (1,0),(0,1) -> 'F'
            | (0,1),(1,0) -> 'F'
            | (-1,0),(0,-1) -> 'J'
            | (0,-1),(-1,0) -> 'J'
            | (-1,0),(0,1) -> '7'
            | (0,1),(-1,0) -> '7'
            | _ -> failwith <| sprintf "unexpected pair: xy: %A, zw: %A" (x,y) (z,w)
    List.mapi (fun i x ->
               List.mapi (fun j y ->
                          if (i,j) <> start then y
                          else f (i,j) (fst start') (snd start')
                          ) x) data


let x : char list list = fixStart' data2 start (List.head start', (List.tail >> List.head) start')

let data2' = List.mapi (fun i x ->
           List.mapi (fun j y -> if Map.containsKey (i,j) loop then Some y else None) x) x


let rec filterPipes xs =
    match xs with
        | [] -> []
        | Some 'L'::Some 'J'::xs -> filterPipes xs
        | Some 'F'::Some '7'::xs -> filterPipes xs
        | Some 'F'::Some 'J'::xs -> Some 'J' :: filterPipes xs
        | Some 'L'::Some '7'::xs -> Some '7' :: filterPipes xs
        | x::xs -> x::filterPipes xs

let filterLine xs =
    List.filter ((<>) (Some '-')) xs
    |> filterPipes


type Field =
    | Pipe
    | Outside
    | Inside

let rec findFields xs =
    match xs with
        | [] -> []
        | Some x::xs -> Pipe:: findFields xs
        | None::xs ->
            let len = filterLine xs |> List.filter Option.isSome |> List.length
            let result =
                if len % 2 = 0 then Outside else Inside
            result::findFields xs



List.map findFields data2'
|> List.concat
|> List.filter ((=) Inside)
|> List.length
|> printfn "Part 2: %d"


