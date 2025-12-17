#time
open System.IO

let explode (s:string) = [for c in s do yield c]

let neighbors (x,y) =
    [x-1,y-1;
     x-1,y;
     x-1,y+1;
     x,y-1;
     x,y+1;
     x+1,y-1;
     x+1,y;
     x+1,y+1;
     ]

let readFile path =
    File.ReadAllLines path
    |> Array.toList
    |> List.map explode
    |> List.mapi (fun i x -> List.mapi (fun j y -> (i,j),y) x)
    |> List.concat
    |> List.filter (fun (_,v) -> v = '@')
    |> List.map fst
    |> Set.ofList

//let data = readFile "test.txt"
let data = readFile "input.txt"

let lessThanFour map coord =
    let isPaperNeighbor k = Set.contains k map
    neighbors coord
    |> List.filter isPaperNeighbor
    |> (fun x -> List.length x < 4)

let removePart1 map =
    Set.fold (fun c k -> if lessThanFour map k then c+1 else c) 0 map

let remove map =
    Set.fold (fun (c,s) k -> if lessThanFour s k then c+1,(Set.remove k s) else c,s) (0,map) map

let removeAll map =
    let rec removeAll' acc map =
        let (removed,map') = remove map
        if removed = 0 then (acc,map')
        else removeAll' (acc+removed) map'
    removeAll' 0 map

data
|> removePart1
|> printfn "Part 1: %d"

data
|> removeAll
|> fst
|> printfn "Part 2: %d"
