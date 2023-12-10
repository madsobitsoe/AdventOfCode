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

// let data = readFile "test.txt"
// let data = readFile "test2.txt"
let data = readFile "input.txt"



let step' map from =
    // Map.find from map |> snd
    let (_,next') = Map.find from map
    let next = List.filter ((<>) from) next'
    next

let step = step' data


let bfsVisit visited (next,count) =
    match Map.tryFind next visited with
        | None -> Map.add next count visited
        | Some s ->
            if count < s then Map.add next count visited
            else visited

    
let rec bfs (map:Map<(int*int),(Direction * (int*int) list)>)  (queue:((int*int)*int) list) visited =
    match queue with
        | [] -> visited
        | (x,count)::xs ->
            // printfn "Visiting %A" x
            if Map.containsKey x visited then
                // printfn "Already visited %A" x
                bfs map xs visited
            else
                // printfn "First time visiting %A" x
                let visited' = bfsVisit visited (x,count)
                let next =
                    step x
                    |> List.map (fun x -> x,count+1)
                // printfn "Next nodes: %A" next
                bfs map (xs@next) visited'
            


let solve data =
    let start = findStart data
    bfs data [(start,0)] Map.empty
    |> Map.toList
    |> List.map snd
    |> List.max


solve data
|> printfn "Part 1: %d"



