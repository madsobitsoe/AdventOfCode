open System.IO
open System.Text.RegularExpressions

let explode (s:string) : char list = [for c in s do yield c]

let to2dArray (xs : char list list) =
    let len1 = List.length xs
    match xs with
        | [] -> failwith "Empty"
        | x::xs' ->
            let len2 = List.length x
            Array2D.init len1 len2 (fun i j -> int (List.item i xs |> List.item j) - 48)

let readFile file =
    File.ReadAllLines file
    |> List.ofArray
    |> List.map explode
    |> to2dArray 


let data = readFile "input.txt"
let test = readFile "test.txt"


let isLowPoint (y,x) (xs:int[,]) maxy maxx =
    let coords =
        [(y,x-1);(y,x+1);(y-1,x);(y+1,x)]
        |> List.filter (fun (y,x) -> x >= 0 && y >= 0 && x < maxx && y < maxy)
    let elm = xs.[y,x]
    let elms = List.map (fun (y,x) -> xs.[y,x]) coords
    List.forall (fun x -> elm < x) elms


let part1 xs =
    let mutable lowPoints : Set<int*int> = [] |> Set.ofList
    let maxx = Array2D.length2 xs
    let maxy = Array2D.length1 xs

    for y = 0 to Array2D.length1 xs - 1 do
        for x = 0 to Array2D.length2 xs - 1 do
            if isLowPoint (y,x) xs maxy maxx then
                lowPoints <- Set.add (y,x) lowPoints
    


    Set.toList lowPoints
    |> List.map (fun (y,x) -> xs.[y,x])
    |> List.map ((+) 1)
    |> List.sum


part1 data
|> printfn "Solution part 1: %d"


let addNeighbors (xs:int[,]) maxy maxx (points:Set<int*int>) (y,x)    =
    let elm = xs.[y,x]
    let coords =
        [(y,x-1);(y,x+1);(y-1,x);(y+1,x)]
        |> List.filter (fun (y,x) -> x >= 0 && y >= 0 && x < maxx && y < maxy)
    List.fold (fun acc (y',x') -> if (xs.[y',x']) <> 9 && (xs.[y',x'] > elm) then Set.add (y',x') acc else acc) points coords
    
let growRegion region xs maxy maxx =
    List.fold (addNeighbors xs maxy maxx) region (Set.toList region )
    
let getBasinSize y x region xs =
    let mutable lastsize = 0
    let mutable currentSize = 1
    let maxy = Array2D.length1 xs 
    let maxx = Array2D.length2 xs
    let mutable region' = region
    while lastsize < currentSize do
        region' <- growRegion region' xs maxy maxx
        lastsize <- currentSize
        currentSize <- Set.count region'
    
    Set.count region'




// Find basins
let findBasins xs =
    let mutable basins : (Set<(int*int) * int>) = Set.ofList []
    let maxy = Array2D.length1 xs
    let maxx = Array2D.length2 xs
    for y = 0 to maxy - 1 do
        for x = 0 to maxx - 1 do
            let newBasin = getBasinSize y x ([y,x] |> Set.ofList) xs
            basins <- Set.add ((y,x),newBasin) basins

    List.sortByDescending (fun (_,size) -> size) (Set.toList  basins)
    |> List.take 3
    |> List.map (fun (_,size) -> size) |> List.reduce (*)



findBasins data
|> printfn "Solution part 2: %d"
