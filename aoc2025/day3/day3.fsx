open System.IO

let explode (s:string) = [for c in s do yield c]

let readFile path =
    File.ReadAllLines path
    |> Array.toList
    |> List.map explode
    |> List.map (List.map (string >> int))


//let data = readFile "test.txt"
let data = readFile "input.txt"

let rec pickLargestOfN acc n xs =
    match n with
        | 1 -> List.max xs :: acc |> List.rev
        | _ ->
            let xs' =
                let len = List.length xs
                let toPick = len - n + 1
                List.take toPick xs

            let largest = List.max xs'
            let idxRest = List.findIndex ((=) largest) xs |> (+) 1
            let _,rest = List.splitAt idxRest xs
            let acc' = largest::acc
            pickLargestOfN acc' (n-1) rest

let toInt : int list -> int64 =
    List.map string
    >> (fun x -> List.foldBack (+) x "")
    >> int64

let part1 =
    pickLargestOfN [] 2
    >> toInt

let part2 =
    pickLargestOfN [] 12
    >> toInt

data
|> List.map part1
|> List.sum
|> printfn "Part 1: %d"

data
|> List.map part2
|> List.sum
|> printfn "Part 2: %d"
