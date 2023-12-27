open System.IO

let readFile file =
    File.ReadAllLines file
    |> Array.map (fun s -> s.Split ": ")
    |> Array.map (fun arr -> arr.[0], arr.[1].Split " " |> List.ofArray)
    |> List.ofArray

let data = readFile "test.txt"


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

let merge (s:string) t map =
    let st = s + t
    let sEdges = Map.find s map |> Set.toList
    let tEdges = Map.find t map |> Set.toList
    let allEdges = sEdges@tEdges
    let uniqueEdges = List.map fst allEdges |> set
    let map' = Map.remove s map |> Map.remove t
    let newWeights =
        List.groupBy fst allEdges
        |> List.map (fun (n,c) -> n, List.map snd c)
        |> List.map (fun (n,(c:int64 list)) -> n, List.sum c)
        |> set
    let nw' = Set.toList newWeights |> Map
    let map'' = Map.add st newWeights map'
    Map.map (fun k v -> if Set.contains k uniqueEdges then Set.map (fun x -> if fst x = s || fst x = t then (st,Map.find k nw') else x) v else v) map''
    

let connections (name:string, edges : Set<string*int64>) = Set.toList edges |> List.map snd |> List.sum

let connectionsIn map (name:string, edges:Set<string*int64>) =
    Set.toList edges
    |> List.filter (fun (k,v) -> Map.containsKey k map)
    |> List.map snd
    |> List.sum
    

let rec minCutPhase' map map' =
    if Map.count map - 2 <= (Map.count map') then
        let toMerge = Map.filter (fun k v -> not (Map.containsKey k map')) map |> Map.toList
        let a = List.head toMerge |> fst
        let b = (List.tail >> List.head) toMerge |> fst
        merge a b map
    else
        printfn "yup"    
        let l = Map.toList map |> List.filter (fun (k,v) -> not (Map.containsKey k map')) |> List.sortByDescending (connectionsIn map')
        printfn "l: %A" l
        let k,v = List.head l
        Map.add k v map'
        |> minCutPhase' map


let minCutPhase map =
    let l' = Map.toList map |> List.sortByDescending connections
    let a = List.take 1 l' |> Map.ofList
    let l = List.tail l'
    minCutPhase' map a


let rec minCut map =
    if Map.count map > 2 then
        let map' = minCutPhase map
        minCut map'
    else
        map

minCut data'
    
// merge "rzs" "frs" data'
// |> merge "rzsfrs" "cmg"
// |> merge "rzsfrscmg" "qnr"
// |> merge "rzsfrscmgqnr" "pzl"
// |> merge "rzsfrscmgqnrpzl" "nvd"
// |> merge "rzsfrscmgqnrpzlnvd" "lsr"
// |> merge "rzsfrscmgqnrpzlnvdlsr" "rsh"
// |> merge "rzsfrscmgqnrpzlnvdlsrrsh" "hfx"
// |> merge "rzsfrscmgqnrpzlnvdlsrrshhfx" "xhk"
// |> merge "rzsfrscmgqnrpzlnvdlsrrshhfxxhk" "ntq"
// |> merge "bvb" "jqt"
// |> merge "bvbjqt" "lhk"
