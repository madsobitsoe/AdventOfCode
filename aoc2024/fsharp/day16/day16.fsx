open System.IO

type Direction =
    | Up
    | Down
    | Left
    | Right

let explode s = [for c in s do yield c]

let withAllDirs (pos,v) =
    [(Up,pos),v;
     (Down,pos),v;
     (Left,pos),v;
     (Right,pos),v;
     ]

let readFile file =
    File.ReadAllText file
    |> (fun s -> s.Split "\n")
    |> List.ofArray
    |> List.map explode
    |> List.filter ((<>) [])

let toMap data =
    List.mapi (fun i x -> List.mapi (fun j y -> if y <> '#' then Some ((i,j),y) else None) x) data
    |> List.concat 
    |> List.filter ((<>) None)
    |> List.map Option.get
    |> List.collect withAllDirs
    |> Map.ofList
    
let findStart dataMap =
    Map.findKey (fun _ v -> v = 'S') dataMap
    |> snd

let findEnd dataMap =
    Map.findKey (fun _ v -> v = 'E') dataMap
    |> snd

//let data = readFile "test.txt"
//let data = readFile "test2.txt"
let data = readFile "input.txt"

let dataMap = toMap data
let start = findStart dataMap
let stop = findEnd dataMap

    
let turnRight = function
    | Up -> Right
    | Right -> Down
    | Down -> Left
    | Left -> Up

let turnLeft = function
    | Up -> Left
    | Left -> Down
    | Down -> Right
    | Right -> Up

let goForward (i,j) dir =
    match dir with
        | Up -> i-1,j
        | Down -> i+1,j
        | Left -> i,j-1
        | Right -> i,j+1

let getNextToVisit unvisited =
    let next = Set.minElement unvisited
    next,Set.remove next unvisited

let updateCostAndPriority (nodes,unvisited) ((dir,pos),cost) =
    match Map.tryFind (dir,pos) nodes with
        | None -> failwith <| sprintf "Node %A,%A not found" dir pos
        | Some (None) ->
            let unvisited' = Set.add (cost,(dir,pos)) unvisited
            let nodes' = Map.add (dir,pos) (Some cost) nodes
            nodes',unvisited'
        | Some (Some cost') ->
            let newCost = min cost cost'
            let unvisited' = if newCost < cost' then Set.add (newCost,(dir,pos)) unvisited else unvisited
            let nodes' = if newCost < cost' then Map.add (dir,pos) (Some newCost) nodes else nodes
            nodes',unvisited'

let rec findPath nodes unvisited =
    if Set.isEmpty unvisited then nodes
    else
        printfn "Left to visit: %d" <| Set.count unvisited
        let (priority,(dir,current)),unvisited' = getNextToVisit unvisited//getNextToVisit nodes unvisited
        // let unvisited' = Set.remove (dir,current) unvisited
        let currentCost =
            match Map.tryFind (dir,current) nodes with
                | None -> failwith <| sprintf "Current node not found: %A" current
                | Some (None) -> failwith <| sprintf "Current node has no cost: %A" current
                | Some (Some cost) -> cost
        let right = (turnRight dir,current), (currentCost + 1000L)
        let left = (turnLeft dir,current),  currentCost + 1000L
        let forward = (dir,(goForward current dir)),currentCost + 1L

        let nodes',unvisited'' =
            [right;left;forward]
            |> List.filter (fun ((dir,pos),_) -> Map.containsKey (dir,pos) nodes)
            |> List.fold updateCostAndPriority (nodes,unvisited')

        findPath nodes' unvisited''


let nodes = Map.map (fun k v -> None) dataMap |> Map.add (Right,start) (Some 0L)
let unvisited = set [(0L,(Right,start))]

findPath nodes unvisited
|> Map.filter (fun (_,pos) v -> pos = stop)
|> Map.values
|> Seq.map Option.get
|> Seq.min
|> printfn "Part 1: %d"
