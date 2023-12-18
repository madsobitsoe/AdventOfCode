open System.IO

let readFile file =
    File.ReadAllLines file
    |> Array.map (fun s -> s.Split " ")
    |> Array.map (fun x -> Array.head x, (Array.tail >> Array.head) x, (Array.tail >> Array.tail >> Array.head) x)
    |> List.ofArray
    |> List.map (fun (dir,count,color) -> dir, int count, color)


    
let data = readFile "test.txt"
let data = readFile "input.txt"




let step dir last =
    let x,y = last
    match dir with
    | "R" -> x,y+1
    | "L" -> x,y-1
    | "U" -> x-1,y
    | "D" -> x+1,y
    | _ -> failwith <| sprintf "Unexpected dir: %A" dir

// let step' dir n last =
//     let x,y = last
//     match dir with
//     | "R" -> x,y+n
//     | "L" -> x,y-n
//     | "U" -> x-n,y
//     | "D" -> x+n,y
//     | _ -> failwith <| sprintf "Unexpected dir: %A" dir



let rec toMap lastPos instructions map =
    match instructions with
        | [] -> map |> List.rev
        | (_, 0, _)::xs -> toMap lastPos xs map
        | (dir,n,color)::xs ->
            let next = step dir lastPos
            toMap next ((dir,n-1,color)::xs) ((lastPos,color)::map)

// let rec toMap' lastPos instructions map =
//     match instructions with
//         | [] -> map // |> List.rev
//         // | (_, 0, _)::xs -> toMap lastPos xs map
//         | (dir,n,color)::xs ->
//             let next = step' dir n lastPos
//             toMap next xs ((lastPos,color)::map)


let data' = toMap (0,0) data []

let minRow = List.minBy (fst >> fst) data' |> fst |> fst
let minCol = List.minBy (fst >> snd) data' |> fst |> snd

let mapper data = List.map (fun ((x,y),color) -> (x + abs minRow, y + abs minCol),color) data
let data'' = mapper data'
let maxRow = List.maxBy (fst >> fst) data'' |> fst |> fst
let maxCol = List.maxBy (fst >> snd) data'' |> fst |> snd

// let mapData = Map.ofList data''

// let data' = toMap' (0,0) data []

// let minRow = List.minBy (fst >> fst) data' |> fst |> fst
// let minCol = List.minBy (fst >> snd) data' |> fst |> snd

// let mapper data = List.map (fun ((x,y),color) -> (x + abs minRow, y + abs minCol),color) data
// let data'' = mapper data'
// let maxRow = List.maxBy (fst >> fst) data'' |> fst |> fst
// let maxCol = List.maxBy (fst >> snd) data'' |> fst |> snd

// data'' |> List.rev

let mapData = Map.ofList data''


let rec fixUpRow row =
    match row with
        | [] -> []
        | x::[] -> x::[]
        | (i,j)::(x,y)::(z,w)::xs ->
            if j+1 = y && y+1 = w then (i,j) :: fixUpRow ((z,w)::xs)
            else (i,j)::fixUpRow((x,y)::(z,w)::xs)
        | (i,j)::(x,y)::xs ->
            if (j+1) = y then (i,j)::fixUpRow xs
            else (i,j)::fixUpRow ((x,y)::xs)

let rec fixUpCol col =
    match col with
        | [] -> []
        | x::[] -> x::[]
        | (i,j)::(x,y)::(z,w)::xs ->
            if i+1 = x && x+1 = z then (i,j) :: fixUpCol ((z,w)::xs)
            else (i,j)::fixUpCol((x,y)::(z,w)::xs)
        | (i,j)::(x,y)::xs ->
            if (i+1) = x then (i,j)::fixUpCol xs
            else (i,j)::fixUpCol ((x,y)::xs)
        // | (i,j)::(x,y)::xs ->
        //     if (i+1) = x then (i,j):: List.tail (fixUpCol ((x,y)::xs))
        //     else (i,j) :: fixUpCol ((x,y)::xs)

fixUpRow [(0,0);(0,1);(0,2);(0,3);(0,4); (0,6)]
fixUpCol [(0,0);(1,0);(2,0);(4,0)]
            
let isInside map i j =
    let row = Map.filter (fun (x,y) v -> i = x) map
    let col = Map.filter (fun (x,y) v -> j = y) map
    let left = Map.filter (fun (x,y) v -> y < j) row |> Map.keys |> List.ofSeq |> List.sortBy snd
    let right = Map.filter (fun (x,y) v -> y > j) row |> Map.keys |> List.ofSeq |> List.sortBy snd
    
    let up = Map.filter (fun (x,y) v -> x < i) col |> Map.keys |> List.ofSeq |> List.sortBy fst
    let down = Map.filter (fun (x,y) v -> x > i) col |> Map.keys |> List.ofSeq |>  List.sortBy fst

    // let lc = left |> List.length
    // let rc = right |> List.length
    // let uc = up |> List.length
    // let dc = down |> List.length

    let lc = fixUpRow left |> List.length
    let rc = fixUpRow right |> List.length
    let uc = fixUpCol up |> List.length
    let dc = fixUpCol down |> List.length
    let res =
        (lc > 0 && rc > 0 && rc % 2 = 1) && (uc > 0 && dc > 0 && dc % 2 = 1)
        
    if res then 1 else 0


let rec fillOutside xs =
    match xs with
        | [] -> []
        | 0::xs -> 0::fillOutside xs
        | 1::xs -> 1::fillInside xs
        | _ -> failwith "yup"
and fillInside xs =
    match xs with
        | [] -> []
        | 1::1::xs -> 1::fillInside (1::xs)
        | 1::0::xs -> 1::0::fillOutside xs
        | 0::xs -> 1::fillInside xs
        | 1::xs -> fillOutside (1::xs)
        | _ -> failwith "yupyup"




List.mapi (fun i x -> List.mapi (fun j y -> match Map.tryFind (i,j) mapData with | None -> 0 | Some _ -> 1) [0..maxCol]) [0..maxRow]
|> List.map fillOutside


List.mapi (fun i x -> List.mapi (fun j y -> match Map.tryFind (i,j) mapData with | None -> isInside mapData i j | Some _ -> 1) [0..maxCol]) [0..maxRow]
|> List.map List.sum
|> List.sum
|> printfn "Part 1: %d"
