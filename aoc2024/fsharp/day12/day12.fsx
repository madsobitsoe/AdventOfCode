open System.IO

let explode s = [for c in s do yield c]

let readFile file =
    File.ReadAllText file
    |> (fun s -> s.Split "\n")
    |> List.ofArray
    |> List.map explode
    |> List.filter ((<>) [])

let toMap data =
    List.mapi (fun i x -> List.mapi (fun j y -> (i,j),(y, set [(i,j)])) x) data
    |> List.concat 
    |> Map.ofList

let neighbors (i,j) =
    [(i-1,j);
     (i+1,j);
     (i,j-1);
     (i,j+1);
        ]

let rec buildRegion garden toVisit visited currentLabel region =
    match toVisit with
        | [] -> visited,region
        | x::xs ->
            if Set.contains x visited then buildRegion garden xs visited currentLabel region
            else
            match Map.tryFind x garden with
                | None ->
                    buildRegion garden xs (Set.add x visited) currentLabel region
                | Some (label,s) ->
                    let visited' = Set.add x visited
                    if label <> currentLabel then
                        buildRegion garden xs visited' currentLabel region
                    else
                        // add to region
                        let region' = Set.add x region
                        // find neighbors
                        let n = neighbors x
                        let toVisit' = List.fold (fun acc x -> x :: acc) toVisit n
                        buildRegion garden toVisit' visited' currentLabel region'

let rec buildRegions garden toVisit visited regions =
    match toVisit with
        | [] -> regions
        | x::xs ->
            if Set.contains x visited then
                buildRegions garden xs visited regions
            else
                match Map.tryFind x garden with
                    | None ->
                        buildRegions garden xs (Set.add x visited) regions
                    | Some (label,s) ->
                        let visited',region = buildRegion garden [x] visited label Set.empty
                        buildRegions garden xs (Set.union region visited) ((label,region)::regions)


//let data = readFile "test.txt" |> toMap
let data = readFile "input.txt" |> toMap
let allPositions = Map.keys data |> List.ofSeq
let regions = buildRegions data allPositions Set.empty []


let getFence region plot =
    neighbors plot
    |> List.fold (fun acc x -> if Set.contains x region then acc else acc+1) 0

let getPrice region =
    Set.fold (fun acc x -> acc + getFence region x) 0 region
    |> (*) (Set.count region)

List.map snd regions
|> List.map getPrice
|> List.sum
|> printfn "Part 1: %d"
