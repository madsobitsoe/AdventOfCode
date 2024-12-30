#time
open System.IO

let parseTowels (s:string) =
    s.Split ", " 
    |> Array.filter ((<>) "") 
    |> List.ofArray
    |> Set.ofList

let parsePatterns (s:string) =
    s.Split "\n" 
    |> Array.filter ((<>) "") 
    |> List.ofArray 

let parseData (data : string list) =
    let towels = data.[0] |> parseTowels
    let patterns = data.[1] |> parsePatterns
    towels,patterns

let readFile file =
    File.ReadAllText file
    |> (fun s -> s.Split "\n\n")
    |> List.ofArray
    |> List.filter ((<>) "")
    |> parseData


//let towels,patterns = readFile "test.txt"
let towels,patterns = readFile "input.txt"

let cachePart1 = System.Collections.Concurrent.ConcurrentDictionary<string,bool>()

let rec isPossible (pattern:string) =
    match cachePart1.TryGetValue pattern with
        | true,value -> value
        | false,_ ->
            let res = isPossible' pattern
            cachePart1[pattern] <- res
            res    
and isPossible' (pattern:string) =
    if pattern = "" then true
    else
        Set.filter (fun (t:string) -> pattern.StartsWith t) towels
        |> Set.exists (fun t -> isPossible (pattern.Substring t.Length))

patterns
|> List.filter isPossible
|> List.length
|> printfn "Part 1: %d"

let cachePart2 = System.Collections.Concurrent.ConcurrentDictionary<string,uint64>()

let rec isPossible2 (pattern:string) =
    match cachePart2.TryGetValue pattern with
        | true,value -> value
        | false,_ ->
            let res = isPossible2' pattern
            cachePart2[pattern] <- res
            res    
and isPossible2' (pattern:string) =
    if pattern = "" then 1UL
    else
        Set.filter (fun (t:string) -> pattern.StartsWith t) towels
        |> Set.fold (fun acc t -> acc + isPossible2 (pattern.Substring t.Length)) 0UL

patterns
|> List.fold (fun acc x -> acc + isPossible2 x) 0UL
|> printfn "Part 2: %d"
