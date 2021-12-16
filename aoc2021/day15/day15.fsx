open System.IO
let explode (s:string) : char list = [for c in s do yield c]

let to2dArray (xs : char list list) =
    let len1 = List.length xs
    match xs with
        | [] -> failwith "Empty"
        | x::xs' ->
            let len2 = List.length x
            Array2D.init len1 len2 (fun i j -> uint64 (List.item i xs |> List.item j) - 48UL)

let readFile file =
    File.ReadAllLines file
    |> List.ofArray
    |> List.map explode
    |> to2dArray 

let test = readFile "test.txt"
let data = readFile "input.txt"

let goal xs = Array2D.length1 xs - 1, Array2D.length2 xs - 1

let reconstruct_path cameFrom current =
    let mutable total_path = [current]
    let mutable cur = current
    while Map.containsKey cur cameFrom do
        cur <- Map.find cur cameFrom
        total_path <- cur::total_path
        // total_path.prepend(current)
    total_path

let gscore map node =
    match Map.tryFind node map with
        | Some c -> c
        | None -> 1UL <<< 63


let fscore map node =
    match Map.tryFind node map with
        | Some c -> c
        | None -> 1UL <<< 63

let neighbors maxy maxx (y,x) =
    [y-1,x;y+1,x;y,x-1;y,x+1] |> List.filter (fun (y,x) -> y >= 0 && x >= 0 && y <= maxy && x <= maxx)


// Basically copy/paste from the A* pseudocode on wikipedia
let A_Star start goal (all_nodes:uint64[,]) =
    let nf = neighbors (fst goal) (snd goal) 
    let mutable openSet = set [start]
    
    let mutable cameFrom = Map []

    let mutable gScoreMap = Map [start,0UL]
    let mutable fScoreMap = Map [start,0UL]

    let mutable total_path = []
    while Set.count openSet > 0 do
        let withFScores = Set.map (fun x -> fscore fScoreMap x,x) openSet
        let current = Set.minElement withFScores |> (fun (_,x) -> x)
        if current = goal then
            total_path <- reconstruct_path cameFrom current
            openSet <- set []
        else
            openSet <- Set.remove current openSet

            for n in (nf current) do
                let ny,nx = n
                let tentative_gScore = gscore gScoreMap current + all_nodes.[ny,nx]
                if tentative_gScore < gscore gScoreMap n then
                    cameFrom <- Map.add n current cameFrom
                    gScoreMap <- Map.add n tentative_gScore gScoreMap
                    fScoreMap <- Map.add n tentative_gScore fScoreMap 
                    if not (Set.contains n openSet) then
                        openSet <- Set.add n openSet
    total_path



let sumPath (xs:uint64[,]) path =
    List.tail path
    |> List.map (fun (y,x) -> xs.[y,x])
    |> List.sum


let p1goal = goal data
A_Star (0,0) p1goal data
|> sumPath data
|> printfn "Solution part 1: %A"

let wrap n = if n+1UL > 9UL then 1UL else n+1UL
    
let toBigCave (xs:uint64[,]) =
    let y,x = Array2D.length1 xs , Array2D.length2 xs
    let origY,origX = y-1,x-1
    let newY,newX = 5 * y, 5 * x
    let mutable start = Array2D.init newY newX
                            (fun i j ->
                                 if i <= origY && j <= origX then xs.[i,j]
                                 else 0UL)

    // First grow cave to the right
    for i = 0 to origY do
        for j = x to newX - 1 do
            start.[i,j] <- wrap start.[i,j - x]

    // then grow cave down 4 times
    for i = y to newY - 1 do
        for j = 0 to newX - 1 do
            let value = wrap start.[i - y,j]
            start.[i,j] <- value
            
    start

let big = toBigCave data
let p2goal = goal big
A_Star (0,0) p2goal big
|> sumPath big
|> printfn "Solution part 2: %A"

