open System.IO

let readFile file =
    File.ReadAllLines file
    |> Array.map (fun s -> s.Split " ")
    |> Array.map (fun x -> Array.head x, (Array.tail >> Array.head) x, (Array.tail >> Array.tail >> Array.head) x)
    |> List.ofArray
    |> List.map (fun (dir,count,color) -> dir, int count, color)


    
let data = readFile "test.txt"
//let data = readFile "test2.txt"
//let data = readFile "input.txt"




let step dir last =
    let x,y = last
    match dir with
    | "R" -> x,y+1
    | "L" -> x,y-1
    | "U" -> x-1,y
    | "D" -> x+1,y
    | _ -> failwith <| sprintf "Unexpected dir: %A" dir

let step' dir n last =
    let x,y = last
    match dir with
    | "R" -> x,y+n
    | "L" -> x,y-n
    | "U" -> x-n,y
    | "D" -> x+n,y
    | _ -> failwith <| sprintf "Unexpected dir: %A" dir



let rec toMap lastPos instructions map =
    match instructions with
        | [] -> map |> List.rev
        | (_, 0, _)::xs -> toMap lastPos xs map
        | (dir,n,color)::xs ->
            let next = step dir lastPos
            toMap next ((dir,n-1,color)::xs) ((lastPos,color)::map)

let rec toMap' lastPos instructions map =
    match instructions with
        | [] -> map // |> List.rev
        // | (_, 0, _)::xs -> toMap lastPos xs map
        | (dir,n,color)::xs ->
            let next = step' dir n lastPos
            toMap' next xs ((lastPos,color)::map)


// let data' = toMap (0,0) data []

// let minRow = List.minBy (fst >> fst) data' |> fst |> fst
// let minCol = List.minBy (fst >> snd) data' |> fst |> snd

// let mapper data = List.map (fun ((x,y),color) -> (x + abs minRow, y + abs minCol),color) data
// let data'' = mapper data'
// let maxRow = List.maxBy (fst >> fst) data'' |> fst |> fst
// let maxCol = List.maxBy (fst >> snd) data'' |> fst |> snd

// let mapData = Map.ofList data''

let data' = toMap' (0,0) data []

let minRow = List.minBy (fst >> fst) data' |> fst |> fst
let minCol = List.minBy (fst >> snd) data' |> fst |> snd

let mapper data = List.map (fun ((x,y),color) -> (x + abs minRow, y + abs minCol),color) data
let data'' = mapper data'
let maxRow = List.maxBy (fst >> fst) data'' |> fst |> fst
let maxCol = List.maxBy (fst >> snd) data'' |> fst |> snd

// data'' |> List.rev

let mapData = Map.ofList data''


let shoeLacePart ((x0:float,y0),(x1,y1)) = x0 * y1 - y0 * x1


let d = [2,1; 5,0; 6,4; 4,2; 1,3; 2,1]

let shoelaceData =
    let last = List.head data''
    data'' @ [last]

//data''
shoelaceData
|> List.map fst
|> List.map (fun (a,b) -> float a, float b)
|> List.pairwise
|> List.map shoeLacePart
|> List.sum
|> float
|> (*) 0.5
// |> (+) (List.length data'' |> float)
|> printfn "shoelace: %A"





let p (xs: int list list) =
    printfn ""
    List.iter (fun x -> List.iter (fun x -> let x' = if x = 1 then "#" else "." in printf "%s" x') x; printfn "") xs
    xs

List.mapi (fun i x -> List.mapi (fun j y -> match Map.tryFind (i,j) mapData with | None -> 0 | Some _ -> 1) [0..maxCol]) [0..maxRow]
|> p
// |> List.map fillOutside
// |> p
|> List.map List.sum
|> List.sum
|> printfn "Part 1: %d"

// List.mapi (fun i x -> List.mapi (fun j y -> match Map.tryFind (i,j) mapData with | None -> isInside mapData i j | Some _ -> 1) [0..maxCol]) [0..maxRow]
// |> List.map List.sum
// |> List.sum
// |> printfn "Part 1: %d"
