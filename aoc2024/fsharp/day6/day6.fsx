open System.IO

let explode s = [for c in s do yield c]

let readFile file =
    File.ReadAllText file
    |> (fun s -> s.Split "\n")
    |> List.ofArray
    |> List.map explode
    |> List.filter ((<>) [])

let toMap data =
    List.mapi (fun i x -> List.mapi (fun j y -> if y <> '.' then Some ((i,j),y) else None) x) data
    |> List.concat 
    |> List.filter ((<>) None)
    |> List.map Option.get
    |> Map.ofList

let findStart dataMap =
    Map.findKey (fun _ v -> v = '^') dataMap

type Direction =
    | Up
    | Down
    | Left
    | Right

//let data = readFile "test.txt"
let data = readFile "input.txt"

let dataMap = toMap data
let start = findStart dataMap
let maxx =
    List.head data
    |> List.length
    |> (fun x -> x - 1)
let maxy =
    List.length data
    |> (fun x -> x - 1)

let inBounds' maxx maxy i j =
    i >= 0 && i <= maxx && j >= 0 && j <= maxy

let inBounds = inBounds' maxx maxy

let step' map input =
    let (dir,(i,j)) = input
    if not (inBounds i j) then None
    else
    match dir with
        | Up ->
            match Map.tryFind (i-1,j) map with
                | Some '#' -> Some (input,(Right,(i,j)))
                | _ -> Some(input, (Up,(i-1,j)))
        | Right ->
            match Map.tryFind (i,j+1) map with
                | Some '#' -> Some(input,(Down, (i,j)))
                | _ -> Some (input,(Right,(i,j+1)) ) 
        | Down ->
            match Map.tryFind (i+1,j) map with
                | Some '#' -> Some (input,(Left, (i,j)))
                | _ -> Some (input,(Down,(i+1,j)))
        | Left ->
            match Map.tryFind (i,j-1) map with
                | Some '#' -> Some (input,(Up,(i,j)))
                | _ -> Some (input,(Left, (i,j-1)))

let step = step' dataMap

let path =
   List.unfold step (Up,start)
   |> List.map snd
   |> set

Set.count path
|> printfn "Part 1: %A"

let rec walk map input visited =
    if Set.contains input visited then true
    else
    match step' map input with
        | None -> false
        | Some (cur,next) -> walk map next (Set.add cur visited)

let checkBlock map block =
    let map' = Map.add block '#' map
    walk map' (Up, start) (set [])

let blocks =
    Set.remove start path
    |> Set.filter (checkBlock dataMap)

Set.count blocks
|> printfn "Part 2: %A"
