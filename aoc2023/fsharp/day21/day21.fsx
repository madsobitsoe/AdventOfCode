open System.IO

let readFile file =
    File.ReadAllLines file
    |> List.ofArray
    |> List.map List.ofSeq

let dataToMap data =
    data
    |> List.mapi (fun i x -> List.mapi (fun j y -> (i,j),y) x)
    |> List.concat
    |> Map.ofList


let data'' = readFile "test.txt"
//let data'' = readFile "test2.txt"
//let data'' = readFile "input.txt"
let data' = dataToMap data''

let maxX = List.length data''
let maxY = List.head data'' |> List.length

let start = Map.filter (fun _ v -> v = 'S') data' |> Map.toList |> List.map fst |> set 

let data = data' |> Map.filter (fun _ v -> v = '.' || v = 'S')

let step data (i,j) =
    let neighbors =
        [i-1,j;
         i+1,j;
         i,j-1;
         i,j+1] |> List.filter (fun (i,j) -> Map.containsKey (i,j) data)
    set neighbors

let rec move max i =
    if i < 0 then move max (max + i)
    else if i < max then i
    else i % max
    
let remapCoord' maxX maxY (i,j) =
    let i' = move maxX i
    let j' = move maxY j
    i', j'

let remapCoord = remapCoord' maxX maxY

let steps data ss =
    Set.fold (fun acc x -> acc + (step data x)) (set []) ss


let rec nSteps data n ss =
    match n with
        | 0 -> ss
        | n ->
            steps data ss
            |> nSteps data (n-1)

nSteps data 64 start
|> Set.count
|> printfn "Part 1: %d"


// Part 2

let data2 =
    data
    |> Map.map (fun _ v -> if v = 'S' then 1L else 0L) 
    |> Map.map (fun _ v -> if v = 1L then set [(0L,0L)] else set []) 

let movep2 max i (wi:int64) =
    if i < 0 then max - 1, wi - 1L
    else if i >= max then 0, wi+1L
    else i,wi

let remapP2' maxX maxY ((i,j),(wi:int64,wj:int64)) =
    let i',wi' = movep2 maxX i wi
    let j',wj' = movep2 maxY j wj
    ((i',j'),(wi',wj'))

let remapP2 = remapP2' maxX maxY



let step' (data: Map<(int*int),Set<(int64*int64)>>) (i,j) =
    let neighbors =
        [i-1,j;
         i+1,j;
         i,j-1;
         i,j+1]
         |> List.filter (fun (i,j) -> remapCoord (i,j) |> Map.containsKey <| data)

    let oldWorlds = Map.find (i,j) data
    let withWorlds (i,j) =
        oldWorlds
        |> Set.fold (fun acc x -> (remapP2 ((i,j),x) |> snd) :: acc) []
        |> set
        |> (fun x -> (i,j),x)
    let res = List.map withWorlds neighbors
    set res

// steps' data2 (set [(1,1)])



// let updateMap map ((i,j),(wi,wj)) =
//     match Map.tryFind (i,j) map with
//         | None -> map
//         | Some s ->
//             Map.add (i,j) (Set.add (wi,wj) s) map

let updateMap map ((i,j),(worlds:Set<int64*int64>)) =
    match Map.tryFind (i,j) map with
        | None -> map
        | Some s ->
            printfn "Found %A at (%d,%d), adding %A" s i j worlds
            Map.add (i,j) (worlds + s) map

let removeAllFromMap map (i,j) =
    match Map.tryFind (i,j) map with
        | None -> map
        | Some s ->
            Map.add (i,j) (set []) map

// For each field with step on
// Step in all possible directions in all worlds
// Remove old step

let steps' data ss =
    let steps = Set.fold (fun acc x -> acc + step' data x) (set []) ss
    printfn "steps: %A" steps
    let data' = Set.fold (fun acc x -> removeAllFromMap acc x) data ss
    Set.fold (fun acc x -> updateMap acc x) data' steps


let rec nSteps' data n ss =
    match n with
        | 0 -> data
        | n ->
            let map' = steps' data ss
            let ss' = Map.filter (fun _ v -> v <> Set.empty) map' |> Map.toList |> List.map fst |> set
            nSteps' map' (n-1) ss'



nSteps' data2 10 start
|> Map.map (fun _ v -> Set.count v)
|> Map.toList
|> List.map snd
|> List.sum
|> printfn "Part 2: %d"
