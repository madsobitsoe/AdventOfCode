open System.IO

let splitter (s:string) =
    let s' = s.Split(':')
    let start = Array.head s'
    let tail = s'.[1].Trim().Split(' ') |> Array.filter ((<>) "") |> set
    start,tail


let rec buildMap map xs =
    match xs with
        | [] -> map
        | x::xs ->
            let k,vs = x
            match Map.tryFind k map with
                | None ->
                    let map' = Map.add k vs map
                    buildMap map' xs
                | Some vs' ->
                    let s' = Set.union vs' vs
                    let map' = Map.add k s' map
                    buildMap map' xs

let readFile path =
    File.ReadAllLines path
    |> Array.map splitter
    |> Array.toList
    |> buildMap Map.empty

//let data = readFile "test.txt"
//let data = readFile "test2.txt"
let data = readFile "input.txt"


let rec findPaths map stop current visited path acc =
    if current = stop then
        let path' = current::path |> List.rev 
        path' :: acc
    else
        match Map.tryFind current map with
            | None -> acc
            | Some nodes ->
                let nodes' = Set.filter (fun n -> Set.contains n visited |> not) nodes
                let visited' = Set.add current visited
                Set.fold (fun paths node -> findPaths map stop node visited' (current::path) paths) acc nodes'

let rec findPaths2 map stop current visited mustVisit cache =
    match Map.tryFind (current,mustVisit) cache with
        | Some c -> cache,c
        | None ->
            if current = stop then
                let res = if Set.isEmpty mustVisit then 1L else 0L
                let cache' = Map.add (current,mustVisit) res cache
                cache',res
            else
                match Map.tryFind current map with
                    | None -> failwith <| sprintf "current not in map: %s" current
                    | Some nodes ->
                        let nodes' = Set.filter (fun n -> Set.contains n visited |> not) nodes |> Set.toList |> List.rev
                        let visited' = Set.add current visited
                        let mv = Set.remove current mustVisit
                        let ca,co =
                            List.fold (fun (cache,count) node -> let (cache',count') = findPaths2 map stop node visited'   mv cache in (cache',count+count')) (cache,0L) nodes'
                        let cache' = Map.add (current,mustVisit) co ca
                        cache',co


findPaths data "out" "you" Set.empty [] []
|> List.length
|> printfn "Part 1: %d"

findPaths2 data "out" "svr" Set.empty (set ["fft";"dac"]) Map.empty
|> snd
|> printfn "Part 2: %d"






