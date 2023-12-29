open System.IO

let readFile file =
    File.ReadAllLines file
    |> Array.map Array.ofSeq
    |> Array.mapi (fun i x -> Array.mapi (fun j y -> (i,j),y) x)
    |> Array.concat
    |> List.ofArray
    |> List.filter (fun ((i,j),x) -> x <> '#')
    |> Map.ofList

let data = readFile "test.txt"
let data = readFile "input.txt"


let start = Map.minKeyValue data |> fst
let stop = Map.maxKeyValue data |> fst

let neighbors map (i,j) : ((int * int) * int) list =
    let up =
        match Map.tryFind (i-1,j) map with
            | Some '.' -> Some (i-1,j)
            | _ -> None
    let down =
        match Map.tryFind (i+1,j) map with
            | Some 'v' -> Some (i+1,j)
            | Some '.' -> Some (i+1,j)
            | _ -> None
    let left =
        match Map.tryFind (i,j-1) map with
            | Some '<' -> Some (i,j-1)
            | Some '.' -> Some (i,j-1)
            | _ -> None
    let right =
        match Map.tryFind (i,j+1) map with
            | Some '>' -> Some (i,j+1)
            | Some '.' -> Some (i,j+1)
            | _ -> None

    [up;down;left;right]
    |> List.filter Option.isSome
    |> List.map Option.get
    |> List.map (fun x -> x,1)
    


let data' = Map.map (fun k v -> neighbors data k) data
let data'l = Map.toList data'

let rec contractAll map =
    let l = Map.toList map
    let res = List.fold (fun acc x -> contract acc x) map l
    if res = map then map
    else
        printfn "Contracting!"
        contractAll res


contractAll data'

let contract map entry =
    printfn "Contracting: %A" entry
    match entry with
        | coord,[(n1,c1);(n2,c2)] ->
            // Ensure both n1 and n2 are still present in the map
            if (not (Map.containsKey n1 map)) || (not (Map.containsKey n2 map)) then map
            else
            // Remove this entry and update n1 and n2 entries
            let n1' =
                match Map.tryFind n1 map with
                | None -> failwith <| sprintf "%A not found in map!" n1
                | Some entries ->
                    match List.tryFind (fun x -> fst x = coord) entries with
                        | None -> entries
                        | Some (entry',c') -> 
                            let without = List.filter (fun x -> fst x <> coord) entries
                            (n2,c2+c')::without
            let n2' =
                match Map.tryFind n2 map with
                | None -> failwith <| sprintf "%A not found in map!" n2
                | Some entries ->
                    match List.tryFind (fun x -> fst x = coord) entries with
                        | None -> entries
                        | Some (entry',c') ->
                            let without = List.filter (fun x -> fst x <> coord) entries
                            (n1,c1+c')::without
            Map.remove coord map
            |> Map.add n1 n1'
            |> Map.add n2 n2'
        | _ ->
            printfn "Skipping"
            map



let m = [(0,1), [(1,1),1]; (1,1), [(0,1),1;(2,1),1]; (2,1), [(1,1),1]] |> Map.ofList
let l = Map.toList m
List.fold (fun acc x -> contract acc x) m l



let rec dfs map steps start stop newMap visited =
    if Set.contains start visited then newMap
    else if start = stop then
        printfn "Found stop after %d steps" steps
        match Map.tryFind start newMap with
            | None -> Map.add start steps newMap
            | Some step -> Map.add start (max step steps) newMap

    else
        let visited' = Set.add start visited
        match Map.tryFind start newMap with
        | Some step ->
            let newMap' =
                if steps < step then newMap
                else Map.add start steps newMap
            // if steps < step then newMap
            // else
                // let newMap' = Map.add start steps newMap
            let queue = Map.find start map |> List.filter (fun x -> not (Set.contains x visited'))
            List.fold (fun acc x -> dfs map (steps+1) x stop acc visited') newMap' queue

        | None ->
            let newMap' = Map.add start steps newMap
            let queue = Map.find start map |> List.filter (fun x -> not (Set.contains x visited'))
            List.fold (fun acc x -> dfs map (steps+1) x stop acc visited') newMap' queue
        
                
dfs data' 0 start stop Map.empty Set.empty
|> Map.find stop
|> printfn "Part 1: %d steps"

// Part 2
let p2data' = Map.map (fun k v -> '.') data
let p2data = Map.map (fun k v -> neighbors p2data' k) p2data'

dfs p2data 0 start stop Map.empty Set.empty
// |> Map.values
// |> Seq.max
|> Map.find stop
|> printfn "Part 2: %d steps"
