#time
open System.IO

let explode s = [for c in s do yield c]

let readFile file =
    File.ReadAllLines file
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

// let data = readFile "testE.txt" |> toMap
// let data = readFile "testAbba.txt" |> toMap                        
//let data = readFile "test.txt" |> toMap
let data = readFile "input.txt" |> toMap

let regions =
    let allPositions = Map.keys data |> List.ofSeq
    buildRegions data allPositions Set.empty []


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


let countCornersForPoint region (i,j) =
    let nw = [i-1,j; i,j-1]
    let nwDiag = i-1,j-1
    let ne = [i-1,j; i,j+1]
    let neDiag = i-1,j+1
    let sw = [i,j-1; i+1,j]
    let swDiag = i+1,j-1
    let se = [i,j+1; i+1,j]
    let seDiag = i+1,j+1
    let exteriorCorners =
        [nw;ne;sw;se]
        |> List.filter (fun x -> List.forall (fun x' -> not (Set.contains x' region)) x)
        |> List.length
    let isInteriorCorner (l,d) = List.forall (fun x -> Set.contains x region) l && not (Set.contains d region)
    let interiorCorners =
        [nw,nwDiag; ne,neDiag; sw,swDiag; se,seDiag]
        |> List.filter isInteriorCorner
        |> List.length
    exteriorCorners + interiorCorners

let countCornersForRegion region =
    Set.fold (fun acc x -> acc + countCornersForPoint region x) 0 region
    
regions
|> List.map (fun (label,region) -> countCornersForRegion region * Set.count region)
|> List.sum
|> printfn "Part 2: %d"
