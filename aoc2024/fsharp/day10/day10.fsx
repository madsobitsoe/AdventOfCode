open System.IO

let explode (s:string) = [for c in s do yield c]

let charsToMap xs =
    xs
    |> List.mapi (fun i x -> List.mapi (fun j y -> ((i,j),(int y)-48)) x)
    |> List.concat
    |> Map.ofList

let readFile file =
    File.ReadAllText file
    |> (fun s -> s.Split '\n')
    |> List.ofArray
    |> List.filter ((<>) "")
    |> List.map explode
    |> charsToMap

let data = readFile "input.txt"

let startPositions =
    data
    |> Map.filter (fun _ v -> v = 0) |> Map.keys |> Seq.toList

let isValidPos map pos h =
    match Map.tryFind pos map with
        | Some x when x = h -> true
        | _ -> false

let step (i,j) (x,y) = i+x, j+y

let rec score map h pos =
    if not (isValidPos map pos h) then [None]
    else if h = 9 then [Some pos]
    else
        let dirs = [(0,1); (0,-1); (1,0); (-1,0)]
        List.map (step pos) dirs
        |> List.collect (score map (h+1))
        |> List.filter Option.isSome

let scores =
    startPositions
    |> List.map (score data 0)

scores
|> List.map Set.ofList
|> List.map Set.count
|> List.sum
|> printfn "Part 1: %A"

scores
|> List.map List.length
|> List.sum
|> printfn "Part 2: %A"

(*
// Stupid way of solving part 2 by finding all actual paths through the graph
let rec rating map start h =
    match isValidPos map start h with
        | None -> [None]
        | Some x ->
            if h = 9 then [Some [start]]
            else
                let (i,j) = start
                let right = rating map (i,j+1) (h+1)
                let left = rating map (i,j-1) (h+1)
                let down = rating map (i+1,j) (h+1)
                let up = rating map (i-1,j) (h+1)

                right @ left @ down @ up
                |> List.filter Option.isSome
                |> List.map Option.get
                |> List.map (fun x -> Some (start::x))


let cleanup xs = xs |> List.filter Option.isSome |> List.map Option.get |> Set.ofList |> Set.count


startPositions
|>List.map (fun x -> rating data x 0 |> cleanup)
|> List.sum
|> printfn "Part 2: %A"

*)
