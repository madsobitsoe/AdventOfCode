open System.IO

let explode (s:string) = [for c in s do yield c]

let readFile file =
    File.ReadAllText file
    |>  explode
    |> List.filter ((<>) '\n')


// let addToMap map x =
//     let k,v = x
//     match Map.tryFind k map with
//         | None -> Map.add k [v] map
//         | Some vs -> Map.add k (v::vs) map

// let toMap data =
//     List.mapi (fun i x -> List.mapi (fun j y -> if y <> '.' then Some (y, (i,j)) else None) x) data
//     |> List.concat 
//     |> List.filter ((<>) None)
//     |> List.map Option.get
//     |> List.fold (fun map x -> addToMap map x) Map.empty 

// //let data = readFile "test.txt"
// let data = readFile "input.txt"


// let dataMap = toMap data
// let maxx =
//     List.head data
//     |> List.length
//     |> (fun x -> x - 1)
// let maxy =
//     List.length data
//     |> (fun x -> x - 1)

// let inBounds' maxx maxy (i, j) =
//     i >= 0 && i <= maxx && j >= 0 && j <= maxy

// let inBounds = inBounds' maxx maxy


let data = readFile "test.txt"
let data = readFile "test1.txt"
let data = readFile "input.txt"


let rec toFileMap (xs:char list) (acc) isFileData idx =
    match xs with
    | [] -> acc
    | x::xs ->
        let xi = (int x) - 48
        let ci = Some idx
        if isFileData then
            toFileMap xs (acc @ List.replicate xi ci) false (idx+1)
        else
            toFileMap xs (acc @ List.replicate xi None) true idx
            
    // | [x] -> failwith <| sprintf "bad data: %A" x
    // | file::empty::xs ->
    //     let f = List.replicate file file
    //     let e = List.replicate empty 0
    //     toFileMap xs (acc @ f @ e)



let fileMap =
    toFileMap data [] true 0
    |> List.toArray

let compact arr =
    let mutable front = 0
    let mutable back = (Array.length arr) - 1
    while front < back do
        match arr.[front] with
            | Some _ ->
                front <- front + 1
            | None ->
                match arr.[back] with
                    | None -> back <- back - 1
                    | Some elm ->
                        arr.[front] <- Some elm
                        arr.[back] <- None
                        back <- back - 1
                        front <- front + 1
    arr



let checkSum arr = arr |> Array.mapi (fun i v -> match v with None -> 0L | Some v -> i * v |> int64) |> Array.reduce (+)

compact fileMap
|> checkSum



// Part 2
type Block =
    | File of int * int
    | Empty of int 

let rec toBlockMap (xs:char list) acc isFileData idx =
    match xs with
        | [] -> acc |> List.rev
        | x :: xs ->
            let xi = (int x) - 48
            let ci = idx
            if isFileData then
                let file = File (xi, ci)
                toBlockMap xs (file :: acc) false (idx+1)
            else
                let empty = Empty xi
                toBlockMap xs (empty::acc) true (idx)




let compact2 arr =
    let mutable front = 0
    let mutable back = (Array.length arr) - 1
    while back >= 0 do
        match arr.[back] with
            | Empty _ ->
                printfn "Empty, back: %A" back
                back <- back - 1
            | File (len,i) ->
                printfn "File: (len: %A, i: %A), back: %A" len i back
                let mutable notFinished = true
                let mutable front = 0
                while notFinished do
                    match arr.[front] with
                        | Empty space ->
                            printfn "Front was empty, %A, empty space: %A" front space
                            if space >= len then
                                arr.[front] <- File (len,i)
                                arr.[back] <- Empty space
                                back <- back - 1
                                notFinished <- false
                            else
                                front <- front + 1
                        | File _ ->
                            printfn "Front had file: %A" front
                            front <- front + 1
                            if front >= back then notFinished <- false
                back <- back - 1
    arr


let blockMap =
    toBlockMap data [] true 0
    |> List.toArray

let compacted = compact2 blockMap

