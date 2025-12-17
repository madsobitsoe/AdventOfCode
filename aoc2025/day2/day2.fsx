open System.IO

let explode (s:string) = [for c in s do yield c]

let readFile path =
    File.ReadAllText path
    |>  (fun (s:string) -> s.Replace("\n","").Split(","))
    |> Array.toList
    |> List.map (fun (s:string) -> let parts = s.Split("-") in parts.[0] |> int64,parts.[1] |> int64)

//let data = readFile "test.txt"
let data = readFile "input.txt"

let expand =
    List.unfold (fun (low,high) -> if low > high then None else Some (sprintf "%d" low, (low+1L,high)))

let fullRange =
    data
    |> List.collect expand

let validPart1 (pattern:string) =
    if pattern.Length % 2 <> 0 then false
    else
        let [a;b] = Seq.splitInto 2 pattern |> Seq.take 2 |> Seq.toList
        a = b

fullRange
|> List.filter validPart1
|> List.map int64
|> List.sum
|> printfn "Part 1: %d"



// Part 2
let buildPrefixTable (pattern:string) =
    let mutable prefixTable = Array.init pattern.Length (fun _ -> 0)
    let mutable prefixLength = 0
    for i = 1 to pattern.Length - 1 do
        while prefixLength > 0 && pattern.[i] <> pattern.[prefixLength] do
            prefixLength <- prefixTable.[prefixLength - 1]

        // If chars match extend prefix
        if pattern.[i] = pattern.[prefixLength] then
            prefixLength <- prefixLength + 1

        prefixTable.[i] <- prefixLength
    prefixTable


let valid pattern =
    let prefixTable = buildPrefixTable pattern
    let lastPrefixLength = Array.last prefixTable
    let potentialSubstringLength = pattern.Length - lastPrefixLength
    lastPrefixLength <> 0 && (pattern.Length % potentialSubstringLength = 0)


fullRange
|> List.filter valid
|> List.map int64
|> List.sum
|> printfn "Part 2 using prefix-table: %d"


// Solve part 2 using permutations of lists
let validByPermutation permuteBy xs =
    let len = List.length xs
    if len / 2 < permuteBy then false
    else
    let permuted =
        xs
        |> List.permute (fun x -> (x + permuteBy) % len)
    xs = permuted

let validByAnyPermutation xs =
    let rec inner max permuteBy =
        if permuteBy > max then false
        else if validByPermutation permuteBy xs then true
        else inner max (permuteBy+1)
    inner (List.length xs) 1

let isValid = explode >> validByAnyPermutation

fullRange
|> List.filter isValid
|> List.map int64
|> List.sum
|> printfn "Part 2 using permutations: %d"
