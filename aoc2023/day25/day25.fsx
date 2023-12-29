open System.IO

let readFile file =
    File.ReadAllLines file
    |> Array.map (fun s -> s.Split ": ")
    |> Array.map (fun arr -> arr.[0], arr.[1].Split " " |> List.ofArray)
    |> List.ofArray

//let data = readFile "test.txt"
let data = readFile "input.txt"


let toMap'' map (h:string,t:string) =
    let h' =
        match Map.tryFind h map with
        | None -> set []
        | Some edges -> edges
    let t' =
        match Map.tryFind t map with
        | None -> set []
        | Some edges -> edges
    Map.add h (Set.add (t,1L) h') map
    |> Map.add t (Set.add (h,1L) t')

let toMap' map (h:string, t:string list) =
    List.fold (fun acc x -> toMap'' acc (h,x)) map t

let toMap = List.fold toMap' Map.empty
let data' = toMap data

let connectionsIn map (name:string, edges:Set<string*int64>) =
    Set.toList edges
    |> List.filter (fun (k,v) -> Map.containsKey k map)
    |> List.length

let findMostConnected mapS mapT =
    Map.toSeq mapS
    |> Seq.maxBy (connectionsIn mapT)

let moveNode mapS mapT node =
    let nodeName,edges = node
    match Map.tryFind nodeName mapS with
        | None -> mapS,mapT
        | Some edges' ->
            Map.remove nodeName mapS, Map.add nodeName edges' mapT

let allConnectionsInOtherComponent (map:Map<string,Set<(string*int64)>>) =
    Map.values map
    |> Seq.map (Set.map fst)
    |> Seq.fold (fun acc x -> acc + x) Set.empty
    |> Seq.filter (fun x -> not (Map.containsKey x map))
    |> Seq.length

let isDone mapS mapT =
    let mapSCons = allConnectionsInOtherComponent mapS
    let mapTCons = allConnectionsInOtherComponent mapT
    mapSCons = 3 && mapTCons = 3

let rec findCut mapS mapT =
    if Map.count mapS < 3 then
        findCut mapT mapS
    else if isDone mapS mapT then mapS,mapT
    else
        let node = findMostConnected mapS mapT
        let mapS',mapT' = moveNode mapS mapT node
        findCut mapS' mapT'


findCut data' Map.empty
|> (fun (a,b) -> Map.count a * Map.count b)
|> printfn "Part 1: %d"


