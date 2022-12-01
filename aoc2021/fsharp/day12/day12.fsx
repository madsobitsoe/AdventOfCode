open System.IO

let rec constructEdges currentF acc1 acc2 xs =
    match xs with
        | (xf,xt)::xs ->
            if xf = currentF then constructEdges currentF (xt::acc1) acc2 xs
            else constructEdges xf [] ((currentF,acc1)::acc2) ((xf,xt)::xs)
        | _ -> (currentF,acc1)::acc2 |> Map.ofList

let readFile filename =
    File.ReadAllLines filename
    |> Array.map (fun (s:string) -> s.Split("-"))
    |> Array.map (fun x -> (x.[0],x.[1]),(x.[1],x.[0]))
    |> Array.toList
    |> List.fold (fun acc (x1,x2) -> x1::x2::acc) []
    |> List.sortBy (fun (s,_) -> s)
    |> constructEdges "" [] []




    
let test = readFile "test.txt"
let testmini = readFile "testmini.txt"
let data = readFile "input.txt"


// Part 1    
let rec bfsPart1 current map visited acc1 =
    if current = "end" then (current::acc1) |> List.rev
    else if List.contains current visited && System.Char.IsLower (current.[0]) then []
    else
        match Map.tryFind current map with
            | None -> []
            | Some [] -> []
            | Some (x::xs) ->
                let newVisited = if System.Char.IsLower (current.[0]) then current::visited else visited
                List.map (fun x' -> bfsPart1 x' map (newVisited) (current::acc1)) (x::xs) |> List.concat

let rec toPaths acc1 acc2 xs =
    match xs with
        | "start"::xs -> toPaths ["start"] acc2 xs
        | "end"::xs -> toPaths [] (("end"::acc1)::acc2) xs
        | x::xs -> toPaths  (x::acc1) acc2 xs
        | _ -> acc2
    
bfsPart1 "start" data [] []
|> toPaths [] []
|> List.distinct
|> List.length
|> printfn "Solution part 1: %d"




// Part 2
// We can visit a single small cave twice
// Start and end, only once each
let rec bfs' current map visited acc1 freebie =
    if current = "end" then (current::acc1) |> List.rev
    else if List.contains current visited && System.Char.IsLower (current.[0]) then []
    else
        match Map.tryFind current map with
            | None -> []
            | Some [] -> []
            | Some (x::xs) ->
                // Avoid visiting start again
                let properNext =
                    List.filter (fun s -> s <> "start") (x::xs)
                // We should both use the freebie if possible
                let newVisited1,free1 =
                    if freebie && System.Char.IsLower (current.[0]) && current <> "start" then
                        visited,false
                    // We do not have a freebie
                    else if System.Char.IsLower (current.[0]) && current <> "start" then
                        current::visited,freebie
                    else
                        visited,freebie
                // And not use it
                let newVisited2,free2 =
                    if System.Char.IsLower (current.[0]) && current <> "start" then
                        current::visited,freebie
                    else visited,freebie
                if freebie then
                    let l1 =
                        List.map (fun x' -> bfs' x' map (newVisited1) (current::acc1) free1) (properNext)
                        |> List.concat
                    let l2 =
                        List.map (fun x' -> bfs' x' map (newVisited2) (current::acc1) free2) (properNext)
                        |> List.concat
                    l1 @ l2
                else
                    List.map (fun x' -> bfs' x' map (newVisited2) (current::acc1) free2) (properNext)
                    |> List.concat
                
bfs' "start" data [] [] true
|> toPaths [] []
|> List.distinct
|> List.length
|> printfn "Solution part 2: %d"


