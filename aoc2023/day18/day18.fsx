open System.IO

let readFile file =
    File.ReadAllLines file
    |> Array.map (fun s -> s.Split " ")
    |> Array.map (fun x -> Array.head x, (Array.tail >> Array.head) x, (Array.tail >> Array.tail >> Array.head) x)
    |> List.ofArray
    |> List.map (fun (dir,count,color) -> dir, int count, color)


//let data = readFile "test.txt"
let data = readFile "input.txt"

let step dir n last =
    let x,y = last
    match dir with
    | "R" -> x,y+n
    | "L" -> x,y-n
    | "U" -> x-n,y
    | "D" -> x+n,y
    | _ -> failwith <| sprintf "Unexpected dir: %A" dir


let rec toMap lastPos instructions map =
    match instructions with
        | [] -> map
        | (dir,n,color)::xs ->
            let next = step dir n lastPos
            toMap next xs ((lastPos,color)::map)


let data' = toMap (0,0) data []

let minRow = List.minBy (fst >> fst) data' |> fst |> fst
let minCol = List.minBy (fst >> snd) data' |> fst |> snd

let mapper data = List.map (fun ((x,y),color) -> (x + abs minRow, y + abs minCol),color) data
let data'' = mapper data'
let maxRow = List.maxBy (fst >> fst) data'' |> fst |> fst
let maxCol = List.maxBy (fst >> snd) data'' |> fst |> snd

let mapData = Map.ofList data''


let shoeLacePart ((x0:float,y0),(x1,y1)) = x0 * y1 - y0 * x1

let shoelaceData =
    let last = List.head data''
    data'' @ [last]


let shoelaceCount =
    shoelaceData
    |> List.map fst
    |> List.map (fun (a,b) -> float a, float b)
    |> List.pairwise
    |> List.map shoeLacePart
    |> List.sum
    |> float
    |> (*) 0.5


let boundaryCount =
    shoelaceData
    |> List.map fst
    |> List.pairwise
    |> List.fold (fun acc ((x0,y0),(x1,y1)) -> acc + (abs (x0-x1)) + (abs (y0-y1))) 0
    |> (fun x -> x / 2 + 1)


shoelaceCount
|> int
|> (+) boundaryCount
|> printfn "Part 1: %d"


// Part 2

let hexToInt64 s = System.Convert.ToInt64(s, 16)

let colorToInstruction (s:string) =
    let num = Seq.skip 2 s |> Seq.take 5 |> Seq.map string |> Seq.reduce (+) |> hexToInt64
    let dir =
        Seq.skip 7 s |> Seq.head
        |> (fun c -> match c with | '0' -> "R" | '1' -> "D" | '2' -> "L" | '3' -> "U" | _ -> failwith <| sprintf "unexpected digit: %A" c)
    dir,num,s
    

let step' (dir:string) (n:int64) (last:int64*int64) =
    let x,y = last
    match dir with
    | "R" -> x,y+n
    | "L" -> x,y-n
    | "U" -> x-n,y
    | "D" -> x+n,y
    | _ -> failwith <| sprintf "Unexpected dir: %A" dir



let rec toMap' lastPos instructions map =
    match instructions with
        | [] -> map
        | (dir,n,color)::xs ->
            let dir,n,color = colorToInstruction color
            let next = step' dir n lastPos
            toMap' next xs ((lastPos,color)::map)



let p2data' = toMap' (0L,0L) data []

let p2minRow = List.minBy (fst >> fst) p2data' |> fst |> fst
let p2minCol = List.minBy (fst >> snd) p2data' |> fst |> snd

let p2mapper data = List.map (fun ((x,y),color) -> (x + abs p2minRow, y + abs p2minCol),color) data
let p2data'' = p2mapper p2data'
let p2maxRow = List.maxBy (fst >> fst) p2data'' |> fst |> fst
let p2maxCol = List.maxBy (fst >> snd) p2data'' |> fst |> snd

let p2mapData = Map.ofList p2data''

let p2shoelaceData =
    let last = List.head p2data''
    p2data'' @ [last]


let p2shoelaceCount =
    p2shoelaceData
    |> List.map fst
    |> List.map (fun (a,b) -> float a, float b)
    |> List.pairwise
    |> List.map shoeLacePart
    |> List.sum
    |> float
    |> (*) 0.5


let p2boundaryCount =
    p2shoelaceData
    |> List.map fst
    |> List.map (fun (a,b) -> int64 a, int64 b)
    |> List.pairwise
    |> List.fold (fun acc ((x0,y0),(x1,y1)) -> acc + (abs (x0-x1)) + (abs (y0-y1))) 0L
    |> (fun x -> x / 2L + 1L)


p2shoelaceCount
|> int64
|> (+) p2boundaryCount
|> printfn "Part 2: %d"
