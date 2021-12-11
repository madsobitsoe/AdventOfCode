open System.IO

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

let test = readFile "test.txt"
let data = readFile "input.txt"

let neighbors xs (x,y) =
    seq { for x' in x - 1 .. x + 1  do
          for y' in y-1 .. y+1 do
          if
              x' >= 0 && x' < Array2D.length2 xs
              && y' >= 0 && y' < Array2D.length1 xs
              && (x,y) <> (x', y')
          then yield (x' ,y') } |> List.ofSeq

let increaseEnergy (x,y) (xs:int[,]) =
    xs.[x,y] <- xs.[x,y] + 1 

let flash (x,y) (xs:int[,]) =
    if (xs.[x,y]) > 9 then
        // Mark this cell
        do xs.[x,y] <- -100
        // For each neighbor, inc by 1
        let n = neighbors xs (x,y)
        List.iter (fun pos -> increaseEnergy pos xs) n

let step (xs:int[,]) =
    let copy = Array2D.copy xs    
    // Everyone increases by 1
    do Array2D.iteri (fun x y elm -> increaseEnergy (x,y) copy ) copy

    // if a cell will flash, increase count of neighbor-cells
    // I'm too stupid to implement "recurse until stable", so hardcode 100 iterations
    for i = 1 to 100 do
        Array2D.iteri (fun x y _-> flash (x,y) copy) copy    
    
    let mutable flashes = 0
    Array2D.iteri (fun x y _ ->
                   if copy.[x,y] < 0 || copy.[x,y] > 9
                   then
                       xs.[x,y] <- 0
                       flashes <- flashes + 1
                   else xs.[x,y] <- copy.[x,y]) copy
    flashes

// Part 1
let part1data = Array2D.copy data

let mutable c = 0
for i = 1 to 100 do
    c <- c + step part1data


printfn "Solution part 1: %d" c


// Part 2
let a2forall xs =
    let mutable res = true
    Array2D.iter (fun elm -> res <- elm = 0 && res) xs
    res
let rec allFlash n xs =
    match a2forall xs with
        | true -> n
        | false ->
            step xs |> ignore
            allFlash (n+1) xs

let part2data = Array2D.copy data            

allFlash 0 part2data
|> printfn "Solution part 2: %d"
