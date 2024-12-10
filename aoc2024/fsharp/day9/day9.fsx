open System.IO

let explode (s:string) = [for c in s do yield c]

let readFile file =
    File.ReadAllText file
    |>  explode
    |> List.filter ((<>) '\n')


//let data = readFile "test.txt"
// let data = readFile "test1.txt"
let data = readFile "input.txt"


let rec toFileMap (xs:char list) (acc) isFileData idx =
    match xs with
    | [] -> acc |> List.rev
    | x::xs ->
        let xi = (int x) - 48
        let ci = Some idx
        if isFileData then
            toFileMap xs (List.replicate xi ci @ acc) false (idx+1)
        else
            toFileMap xs (List.replicate xi None @ acc) true idx

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
|> printfn "Part 1: %A"


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



let blockMap =
    toBlockMap data [] true 0

let rec consolidate acc xs =
    match xs with
        | [] -> List.rev acc
        | Empty s1::Empty s2::xs ->
            consolidate  (Empty (s1+s2)::acc) xs
        | x::xs -> consolidate (x::acc) xs

let canFit n x =
    match x with
        | File _ -> false
        | Empty space -> space >= n

let rec insert xs x =
    match x with
        | Empty _ -> xs
        | File (count,value) ->
            match List.tryFindIndex (canFit count) xs with
                | None -> xs
                | Some idx ->
                    match List.item idx xs with
                        | File _ -> failwith <| sprintf "Found file, error"
                        | Empty space ->
                            let orgIdx = List.findIndexBack ((=) x) xs
                            if orgIdx <= idx then xs
                            else if space = count then
                                List.updateAt idx x xs
                                |> List.updateAt orgIdx (Empty space)
                                |> consolidate []
                            else
                                List.updateAt orgIdx (Empty count) xs
                                |> List.removeAt idx
                                |> List.insertManyAt idx [x; Empty (space - count)]
                                |> consolidate []




let part2 xs =
    let acc = xs
    let reversed = List.tail xs |> List.rev
    List.fold insert xs reversed 

let rec toArray acc xs =
    match xs with
        | [] -> List.rev acc |> List.toArray
        | File (count,value)::xs ->
            toArray  (List.replicate count (value |> Some) @ acc) xs
        | Empty space::xs ->
            toArray ((List.replicate space None) @ acc) xs


part2 blockMap
|> toArray []
|> checkSum
|> printfn "Part 2: %A"

