open System.IO
let readFile file  =
    File.ReadAllText file
    |> (fun s -> s.Split(","))
    |> Array.map int
    |> List.ofArray

let data = readFile "input.txt"
let test = readFile "test.txt"

// part 1 
// The solution for part 2 works for part 1 as well
// but since I wrote it and it's much easier to read and understand,
// I'm keeping it here

// Step for one day
let rec step acc = function
    | [] -> acc
    | x::xs ->
        // For each lanternfish we either breed a new fish
        if x = 0 then step (6::8::acc) xs
        // or let it age and go on through the list
        else step (x-1::acc) xs
// We recurse through each day and step as necessary
let rec steps n xs =
    match n with
        | 0 -> xs
        | n -> steps (n-1) (step [] xs)

data
|> steps 80
|> List.length
|> printfn "Part 1 (simple) solution: %d"
        

// part 2
// The approach from part 1 will grow massive in no time and crash the mono runtime
// The count we're looking for is larger than 32 bits as well
// So, shrink the list by storing counts alongside ages, as 64 bit ints            
let getCounts xs = List.countBy id xs |> List.map (fun (a,c) -> a,int64 c)

// And make it possible to merge entries that have the same age
// merge' expects a list of (age,count), sorted by age
let rec merge' (acc: (int * int64) list)  = function
    | [] -> acc
    | [x] -> x::acc
    | x::y::xs ->
        let a1,c1 = x
        let a2,c2 = y
        if a1 = a2 then merge' ((a1,c1 + c2)::acc) xs
        else merge' (x::acc) (y::xs)

// A step function that steps with the new (age,count) type
let rec step' acc = function
    | [] -> acc
    | x::xs ->
        let age,count = x
        if age = 0 then step' ((6,count)::(8,count)::acc) xs
        else step' (((age-1),count)::acc) xs

// And a steps' function, that will call the step' function n times
let rec steps' n xs =
    match n with
        | 0 -> xs
        | n ->
            // We sort by age, merge counts of all equal ages and perform a step
            let next =  List.sortBy (fun (x,c) -> x) xs |> merge' [] |> step' []
            // Then call recursively to either stop or step again
            steps' (n-1) next

// Part 1 - 
data
|> getCounts
|> steps' 80
|> List.map (fun (_,c) -> c)
|> List.sum
|> printfn "Solution part 1 (with part 2 approach): %d"

// part 2
data
|> getCounts
|> steps' 256
|> List.map (fun (_,c) -> c)
|> List.sum
|> printfn "Solution part 2: %d"
