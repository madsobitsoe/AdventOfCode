open System.IO

let explode (s:string) = [|for c in s do yield c|]

let readFile filename =
    File.ReadAllLines filename
    |> Array.map explode
    |> (fun x ->
        let len1 = Array.length x
        let len2 = Array.head x |> Array.length
        Array2D.init len1 len2 (fun i j -> x.[i].[j]))





let step cucumbers =
    let mutable moved = 0
    let len1 = Array2D.length1 cucumbers  - 1
    let len2 = Array2D.length2 cucumbers  - 1
    let mutable toMoveRight = []
    let mutable toMoveDown = []
    // Move right if not blocked
    for y = 0 to len1 do
        for x = 0 to len2 do
            if y = 0 && x = 3 then printfn "%d,%d" y x
            match cucumbers.[y,x] with
                | '>' -> 
                    // Check if cell to the right is empty
                    let x' = if x+1 > len2  then 0 else x+1
                    if cucumbers.[y,x'] = '.' then
                        toMoveRight <- ((y,x),(y,x'))::toMoveRight
                        moved <- moved + 1
                | _ -> ()
    // Actually move stuff, manipulating the array
    for ((y,x),(y',x')) in toMoveRight do
        cucumbers.[y,x] <- '.'
        cucumbers.[y',x'] <- '>'

    // Move down if not blocked
    for x = 0 to len2 do
        for y = 0 to len1 do
            match cucumbers.[y,x] with
                | 'v' -> 
                    // Check if cell below is empty
                    let y' = if y+1 > len1 then 0 else y+1
                    if cucumbers.[y',x] = '.' then
                        toMoveDown <- ((y,x),(y',x))::toMoveDown
                        moved <- moved + 1
                | _ -> ()

    for ((y,x),(y',x')) in toMoveDown do
        cucumbers.[y,x] <- '.'
        cucumbers.[y',x'] <- 'v'
    moved

let rec findStale (n:uint64) cucumbers =
    if step cucumbers = 0 then (n+1UL)
    // else if n > 9999999 then failwith "too many iterations"
    else
        printfn "step %d" n
        findStale (n+1UL) cucumbers



let test = readFile "test.txt"
let test2 = readFile "test2.txt"
let data = readFile "input.txt"

// test

// step test

findStale 0UL data
