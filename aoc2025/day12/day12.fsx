open System.IO

// Embarrassing "cheating" solution that takes a guess at best
let parseShape (s:string) =
    Seq.filter ((=) '#') s |> Seq.length

let parseRegion (s:string) =
    let dim',idxs' = s.Split(":") |> fun x -> Array.head x, Array.tail x |> Array.head
    let dim = dim'.Split('x') |> fun x -> x.[0] |> int, x.[1] |> int
    let idxs = idxs'.Split(" ") |> Array.filter ((<>) "") |> Array.map int |> List.ofArray
    dim,idxs

let rec parseShapes shapes (ss:string list) =
    match ss with
        | [] -> failwith "empty list?"
        | ""::ss -> parseShapes shapes ss
        | s::ss ->
            if s.Contains '#' then
                let shapes' = (parseShape s) :: shapes
                parseShapes shapes' ss
            else
                let next = s.Split "\n" |> Array.filter ((<>) "") |> List.ofArray
                parseRegions next (List.rev shapes) []

and parseRegions rs shapes regions =
    match rs with
        | [] -> shapes,regions
        | ""::rs -> parseRegions rs shapes regions
        | r::rs ->
            parseRegion r :: regions
            |> parseRegions rs shapes

let parseShapesAndRegions = parseShapes  [] 

let readFile path =
    File.ReadAllText path
    |> fun (s:string) -> s.Split("\n\n")
    |> List.ofArray
    |> parseShapesAndRegions



let shapes,regions = readFile "input.txt"

let area (a, b) =
    if a = 0 then b
    else if b = 0 then a
    else a*b

regions
|> List.filter (fun (region,ss) ->
             let size = area region
             let shapesSize = List.mapi (fun i x' -> shapes.[i]  * x') ss |> List.sum
             size >= shapesSize)
|> List.length
|> printfn "Part 1: %d"
