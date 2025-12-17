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
    |> Map.ofList

//let data = readFile "test.txt"
let data = readFile "input.txt"

let lessThanFour map coord =
    let isPaperNeighbor k = Map.tryFind k map
    neighbors coord
    |> List.choose isPaperNeighbor
    |> (fun x -> List.length x < 4)

let remove map =
    Map.fold (fun (c,m) k v -> if lessThanFour map k then c+1,(Map.remove k m) else c,m) (0,map) map

let removeAll map =
    let rec removeAll' acc map =
        let (removed,map') = remove map
        if removed = 0 then (acc,map')
        else removeAll' (acc+removed) map'
    removeAll' 0 map

data
|> remove
|> fst
|> printfn "Part 1: %d"

data
|> removeAll
|> fst
|> printfn "Part 2: %d"
