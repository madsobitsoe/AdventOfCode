#time
open System.IO

let explode (s:string) = [for c in s do yield c]

// Parse all keys and locks to sets of coordinates of '#'
let parseKeyLock (s:string) =
    s.Split "\n"
    |> Array.filter ((<>) "")
    |> List.ofArray
    |> List.map explode
    |> List.mapi (fun i x -> List.mapi (fun j y -> (i,j),y) x)
    |> List.concat
    |> List.filter (fun (k,v) -> v = '#')
    |> List.map fst
    |> Set.ofList
  
let readFile file =
    File.ReadAllText file
    |> fun s -> s.Split "\n\n"
    |> Array.filter ((<>) "")
    |> List.ofArray
    |> List.map parseKeyLock


let isLock x = Set.contains (0,0) x
let isUnique a b =
    Set.intersect a b = Set.empty
    
//let data = readFile "test.txt"
let data = readFile "input.txt"


let folder acc x =
    // If this is a lock we don't care
    if Set.contains (0,0) x then acc
    else
        // Count the number of unique empty intersections between this key and all locks
        data
        |> List.filter (fun x' -> ((isLock x')) && isUnique x x')
        |> Set.ofList
        |> Set.count
        |> int64
        |> (+) acc    

data
|> List.fold folder 0L
|> printfn "Part 1: %d"
