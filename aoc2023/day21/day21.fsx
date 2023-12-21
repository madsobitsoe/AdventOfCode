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

//let data'' = readFile "test.txt"
let data'' = readFile "input.txt"
let data' = dataToMap data''

let maxX = List.length data''
let maxY = List.head data'' |> List.length

let start = Map.filter (fun _ v -> v = 'S') data' |> Map.toList |> List.map fst |> set 

let data =
    data'
    |> Map.filter (fun _ v -> v = '.' || v = 'S')

let data2 = Map.map (fun _ v -> if v = 'S' then 1L else 0L) data


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
    i',j'

let remapCoord = remapCoord' maxX maxY


let rec mapToWorld max i =
    if i >= 0 && i < max then 0L
    else if i >= max then 1L + mapToWorld max (max - i)
    else -1L + mapToWorld max (max + i)

let step' data (i,j) =
    let neighbors =
        [i-1,j;
         i+1,j;
         i,j-1;
         i,j+1]
         |> List.filter (fun (i,j) -> remapCoord (i,j) |> Map.containsKey <| data)
         |> List.map (fun (i,j) -> remapCoord (i,j),(mapToWorld maxX i, mapToWorld maxY j))
    set neighbors

// let step' data (i,j) =
//     let neighbors =
//         [i-1,j;
//          i+1,j;
//          i,j-1;
//          i,j+1]
//          |> List.filter (fun (i,j) -> remapCoord (i,j) |> Map.containsKey <| data)
//          |> List.map (fun (i,j) -> (i,j),1L + (remapCoord (i,j) |> Map.find <| data ))
//     set neighbors


let steps data ss =
    Set.fold (fun acc x -> acc + (step data x)) (set []) ss

let updateMap map ((i,j),(wi,wj)) =
    match Map.tryFind (i,j) map with
        | None -> map //Map.add (i',j') 1L map
        | Some s ->
            Map.add (i,j) (Set.add (wi,wj) s) map

let removeFromMap map ((i,j),(wi,wj)) =
    match Map.tryFind (i,j) map with
        | None -> map
        | Some s ->
            Map.add (i,j) (Set.remove (wi,wj) s) map

let removeAllFromMap map (i,j) =
    match Map.tryFind (i,j) map with
        | None -> map
        | Some s ->
            Map.add (i,j) (set []) map

// let updateMap  map ((i,j),(c:int64)) =
//     let i',j' = remapCoord (i,j)
//     match Map.tryFind (i',j') map with
//         | None -> map //Map.add (i',j') 1L map
//         | Some c' ->
//             Map.add (i',j') (c'+c) map
        
let steps' data ss =
    // printfn "ss: %A" ss
    // Probably here, use a map not a set
    // let sss = List.fold (fun acc x -> acc @ (step' data x |> Set.toList)) [] (ss |> Set.toList)
    let steps = Set.fold (fun acc x -> acc + step' data x) (set []) ss

    // printfn "Steps: %A" steps
    // printfn "sss: %A" sss
    let data' = Set.fold (fun acc x -> removeAllFromMap acc x) data ss
    Set.fold (fun acc x -> updateMap acc x) data' steps


let rec nSteps data n ss =
    match n with
        | 0 -> ss
        | n ->
            steps data ss
            |> nSteps data (n-1)

let rec nSteps' data n ss =
    match n with
        | 0 -> data
        | n ->
            let map' = steps' data ss
            let ss' = Map.filter (fun _ v -> v <> Set.empty) map' |> Map.toList |> List.map fst |> set
            // printfn "ss': %A" ss'
            nSteps' map' (n-1) ss'


let data3 =
    Map.map (fun _ v -> if v = 1L then set [(0L,0L)] else set []) data2


nSteps data 64 start
|> Set.count
|> printfn "Part 1: %d"


nSteps' data3 10 start
|> Map.map (fun _ v -> Set.count v)
|> Map.toList
|> List.map snd
|> List.sum
|> printfn "Part 2: %d"
