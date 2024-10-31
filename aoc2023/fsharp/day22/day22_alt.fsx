open System.IO

let splitter (s:string) =
    s.Split "~"
    |> Array.map (fun s -> s.Split ",")
    |> (fun x -> Array.head x, (Array.tail >> Array.head) x)
    ||> (fun a b -> toTriples a, toTriples b)

let genName' i =
    let iDiv = i / 26
    let iMod = i % 26
    if iDiv = 0 then string ([|'A'..'Z'|].[iMod])
    else string ([|'A'..'Z'|].[iMod]) + string iDiv

let genName = Seq.initInfinite genName' |> Seq.cache


let xz' (x,_,z) = x,z
let yz' (_,y,z) = y,z

let xz = function
    | start,stop -> xz' start, xz' stop
let yz = function
    | start,stop -> yz' start, yz' stop


let triplesToCoords (name, ((x0,y0,z0),(x1,y1,z1))) =
    let x = [x0..x1]
    let y = [y0..y1]
    let z = [z0..z1]
    List.collect (fun x -> List.collect (fun y -> List.map (fun z -> z,((x,y),name)) z) y) x 



let readFile file =
    File.ReadAllLines file
    |> Array.map splitter
    |> List.ofArray

let dataToMap data =
    data
    |> List.mapi (fun i x -> Seq.item i genName,x)
    |> List.collect triplesToCoords
    |> List.groupBy fst
    |> List.map (fun (z,l) -> z,List.map snd l |> Map.ofList)
    |> Map.ofList



let toArrays map maxX maxY maxZ =
//    let arrs = Map.maxKeyValue map |> fst
    let arrs = maxZ+1
    let init2d map' = Array2D.init (maxX+1) (maxY+1) (fun j i -> match Map.tryFind (i,j) map' with | None -> "." | Some name -> name)
    Array.init (arrs) (fun i -> init2d (match Map.tryFind (i+1) map with | None -> Map.empty |Some m -> m))




let canMove (arrays: string array2d []) z x y =
    match z with
        | 0 | 1 -> false
        | z ->
            if arrays.[z-1].[x,y] = "." then false
            else arrays.[z-2].[x,y] = "."

let move (arrays: string array2d []) z x y =
    match z with
        | 0 | 1 -> ()
        | z ->
            if arrays.[z-1].[x,y] = "." then ()
            else
                arrays.[z-2].[x,y] <- arrays.[z-1].[x,y]
                arrays.[z-1].[x,y] <- "."

let moveBlocks (arrays: string array2d []) =
    for z' = 1 to Array.length arrays do
        for z = z'+1 to Array.length arrays do
            let mutable blockMap : Map<string,bool> = Map.empty
            Array2D.iteri (fun i j v ->
                      match Map.tryFind v blockMap with
                      | None -> blockMap <- Map.add v (canMove arrays z i j) blockMap
                      | Some b -> blockMap <- Map.add v (b && canMove arrays z i j) blockMap) arrays.[z-1]
            Array2D.iteri (fun i j v -> if Map.find v blockMap then move arrays z i j else ()) arrays.[z-1]

let canMoveBlocks (arrays: string array2d []) z =
    match z with
        | 0 | 1 -> false
        | _ ->
            let mutable blockMap : Map<string,bool> = Map.empty
            Array2D.iteri (fun i j v ->
                           match Map.tryFind v blockMap with
                           | None -> blockMap <- Map.add v (canMove arrays z i j) blockMap
                           | Some b -> blockMap <- Map.add v (b && canMove arrays z i j) blockMap) arrays.[z-1]
            Map.exists (fun _ v -> v) blockMap

let canMoveAnyBlocks (arrays: string array2d []) =
    let rec check n z =
        if n = z then canMoveBlocks arrays n
        else
            if canMoveBlocks arrays n then true
            else check (n+1) z
    check 2 (Array.length arrays)

let removeBlockAndCheck block (arrays: string array2d []) =
    let copy = Array.map (Array2D.map (fun v -> if v = block then "." else v)) arrays
    if canMoveAnyBlocks copy then 0 else 1

let data = readFile "test.txt"
let data = readFile "input.txt"

let maxX = data |> List.map (fun (_,(x,_,_)) -> x) |> List.max
let maxY = data |> List.map (fun (_,(_,y,_)) -> y) |> List.max
let maxZ = data |> List.map (fun (_,(_,_,z)) -> z) |> List.max

let datamap = dataToMap data
let arrays = toArrays datamap maxX maxY maxZ
let stable = Array.map Array2D.copy arrays
// urgh, imperative, urgh
while canMoveAnyBlocks stable do moveBlocks stable

let getBlockNames (arrays:string array2d []) =
    let mutable names :Set<string> = Set.empty
    arrays |> Array.iter (fun x -> x |> Array2D.iter (fun v -> if v <> "." then names <- Set.add v names else ()))
    names

let blockNames = getBlockNames stable
let solvePart1 =
    Set.fold (fun acc blockname -> acc + removeBlockAndCheck blockname stable) 0 blockNames
    


// Part 2

let canMoveBlocks' (arrays: string array2d []) z =
    match z with
        | 0 | 1 -> false, Set.empty
        | _ ->
            let mutable blockMap : Map<string,bool> = Map.empty
            Array2D.iteri (fun i j v ->
                           match Map.tryFind v blockMap with
                           | None -> blockMap <- Map.add v (canMove arrays z i j) blockMap
                           | Some b -> blockMap <- Map.add v (b && canMove arrays z i j) blockMap) arrays.[z-1]
            Map.exists (fun _ v -> v) blockMap, Map.filter (fun _ v -> v) blockMap |> Map.toSeq |> Seq.map fst |> set

let canMoveAnyBlocks' (arrays: string array2d []) =
    let rec check n z acc =
        if n = (z+1) then acc
        // if n = z then canMoveBlocks' arrays n + acc
        else
            let acc' = acc + (canMoveBlocks' arrays n |> snd)
           
            check (n+1) z acc'
    check 1 (Array.length arrays) Set.empty


let removeBlocks blocks (arrays: string array2d []) =
    Array.map (Array2D.map (fun v -> if Set.contains v blocks then "." else v)) arrays
    
let rec removeBlockAndCheck' block (arrays: string array2d []) =
    let mutable copy = Array.map (Array2D.map (fun v -> if v = block then "." else v)) arrays
    let mutable blocksRes = Set.empty
    let mutable lastRes = canMoveAnyBlocks' copy
    printfn "%A" lastRes
    while (not (Set.isEmpty lastRes)) do
           blocksRes <- blocksRes + lastRes
           copy <- removeBlocks blocksRes copy
           lastRes <- canMoveAnyBlocks' copy

    Set.count blocksRes


let solvePart2 =
    Set.fold (fun acc blockName -> acc + removeBlockAndCheck' blockName stable) 0 blockNames
