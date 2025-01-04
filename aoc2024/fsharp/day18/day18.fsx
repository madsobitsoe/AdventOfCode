#time
open System.IO
open System.Text.RegularExpressions

let (|Integer|_|) (str: string) =
   let mutable intvalue = 0L
   if System.Int64.TryParse(str, &intvalue) then Some(intvalue)
   else None

let getInt = function
    | Integer i -> i
    | x -> failwith <| sprintf "Bad data: %A" x

let (|ParseRegex|_|) regex str =
   let m = Regex(regex).Match(str)
   if m.Success
   then Some (List.tail [ for x in m.Groups -> x.Value ])
   else None

let (|Coord|_|) = function
    | ParseRegex "(\d+),(\d+)" [Integer x; Integer y] -> Some (x,y)
    | _ -> None

let getCoord = function
    | Coord c -> c
    | x -> failwith <| sprintf "Bad data: %A" x
    
let readFile file =
    File.ReadAllText file
    |> (fun s -> s.Split "\n")
    |> List.ofArray
    |> List.filter ((<>) "")
    |> List.mapi (fun i x -> i,getCoord x)

let minx,maxx = 0,70
let miny,maxy = 0,70

// let minx,maxx = 0,6
// let miny,maxy = 0,6


let start = set [0L,(0,0)]
//let goal = 6,6
let goal = 70,70

//let data = readFile "test.txt"
let data = readFile "input.txt"

let inbounds' maxx maxy (i,j) =
    i >= 0 && i <= maxx && j >= 0 && j <= maxy

let inbounds = inbounds' maxx maxy

let simulateDrops (data: (int *(int64*int64)) list) n =
    List.take n data
    |> List.map snd
    |> List.map (fun (x,y) -> (int x, int y))
    |> Set.ofList
    // |> Map.ofList


let neighbors corrupted (i,j) =
    [ (i-1,j);
      (i+1,j);
      (i,j-1);
      (i,j+1);
    ]
    |> List.filter (fun x -> inbounds x && not (Set.contains x corrupted))

let corruptedMap = simulateDrops data 1024
//let corruptedMap = simulateDrops data 12    

let nodes : Map<(int*int),int64 option> =
    [0..maxx]
    |> List.mapi (fun i x -> List.mapi (fun j u -> (i,j),None) [0..maxy])
    |> List.concat
    |> List.filter (fun x -> not (Set.contains (fst x) corruptedMap))
    |> Map.ofList
    |> Map.add (0,0) (Some 0)

let getNextToVisit unvisited =
    let next = Set.minElement unvisited
    let unvisited' = Set.remove next unvisited
    next,unvisited'


let updateCostAndPriority (nodes,unvisited) (pos,cost) =
    match Map.tryFind pos nodes with
        | None -> failwith <| sprintf "Node %A not found" pos
        | Some (None) ->
            let unvisited' = Set.add (cost,pos) unvisited
            let nodes' = Map.add pos (Some cost) nodes
            nodes',unvisited'
        | Some (Some cost') ->
            let newCost = min cost cost'
            let unvisited' = if newCost < cost' then Set.add (newCost,pos) unvisited else unvisited
            let nodes' = if newCost < cost' then Map.add pos (Some newCost) nodes else nodes
            nodes',unvisited'

let rec findPath (avoid:Set<int*int>) (nodes:Map<(int*int),int64 option>) (unvisited: Set<int64*(int*int)>) =
    if Set.isEmpty unvisited then nodes
    else
        let (_,pos),unvisited' = getNextToVisit unvisited
        let currentCost =
            match Map.tryFind pos nodes with
                | None -> failwith <| sprintf "Current node not found: %A" pos
                | Some (None) -> failwith <| sprintf "Current node has no cost: %A" pos
                | Some (Some cost) -> cost
        let neighs = neighbors avoid pos |> List.map (fun x -> x,currentCost+1L)
        let nodes',unvisited'' =
            neighs
            |> List.fold updateCostAndPriority (nodes,unvisited')
        findPath avoid nodes' unvisited''



let after1024 = findPath corruptedMap nodes start //(set [(0L,(0,0))])
after1024
|> Map.find goal
|> printfn "Part 1: %A"


//let restOfTheDrops =  List.skip 12 data |> List.map snd |> List.map (fun (x,y) -> int x, int y)
let restOfTheDrops = List.skip 1024 data |> List.map snd |> List.map (fun (x,y) -> int x, int y)

let rec corruptAndSearch nextDrops avoid nodes =
    match nextDrops with
        | [] -> failwith <| sprintf "Ran out of drops!"
        | d::ds ->
            let avoid' = Set.add d avoid
            let nodes' = Map.remove d nodes
            let unvisited = set [0L,(0,0)]
            let path = findPath avoid' nodes' unvisited
            match Map.tryFind goal path with
                | None -> failwith <| sprintf "Couldn't find goal in path!"
                | Some (None) -> d
                | Some (Some cost) ->
                    // printfn "Dropping byte at %A" d
                    // printfn "found path with cost %A" cost
                    corruptAndSearch ds avoid' nodes'
                
corruptAndSearch restOfTheDrops corruptedMap nodes
|> printfn "Part 2: %A"
