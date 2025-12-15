open System.IO

let readFile path =
    File.ReadAllLines path
    |> Array.map (fun (s:string) -> s.Split(','))
    |> Array.map (fun (s:string[]) -> s.[0] |> string |> int64, s.[1] |> string |> int64)
    |> Array.toList
    |> seq

let data = readFile "test.txt"
let data = readFile "input.txt"

let dist' a b = max a b - min a b + 1L
let dist ((a,b), (c,d)) = dist' a c * dist' b d

let findLargestRect grid =
    let pairs = Seq.allPairs grid grid
    Seq.map dist pairs |> Seq.max

findLargestRect data
|> printfn "Part 1: %d"


// Part 2
let computePointsBetween (a:int64,b:int64) (c:int64,d:int64) =
    let minx,maxx = min a c, max a c
    let miny,maxy = min b d , max b d
    if a = c then
        Some [for y in miny .. maxy do yield a,y]
    else if b = d then
        Some [for x in minx .. maxx do yield x,b]
    else
        None


let fixRange (a,b) = min a b, max a b

let rec joinRanges acc xs =
    match xs with
        | [] -> acc |> List.sort
        | x::[] -> (fixRange x)::acc |> List.sort
        | a::b::xs ->
            let al,ah = fixRange a
            let bl,bh = fixRange b
            if ah >= bl then
                let l,h = min al bl, max ah bh
                joinRanges acc ((l,h)::xs)
            else if al = ah && bl = bh then
                joinRanges acc ((al,bh)::xs)
            else if al = ah then
                joinRanges acc ((al,bh)::xs)
            else if al <= bl && ah >= bl then
                joinRanges acc ((al,max ah bh)::xs)
            else joinRanges ((al,ah)::acc) ((bl,bh)::xs)



let rec buildMap map xs =
    match xs with
        | [] -> map
        | ((x1,y1),(x2,y2))::xs when y1 = y2 ->
            match Map.tryFind y1 map with
                | Some v ->
                    let newRanges = joinRanges [] (((x1,x2)::v |> List.sort))
                    let map' = Map.add y1 newRanges map
                    buildMap map' xs
                | None ->
                    let map' = Map.add y1 [(x1,x2)] map
                    buildMap map' xs
        | ((x1,y1),(x2,y2))::xs when x1 = x2 ->
            let p = computePointsBetween (x1,y1) (x2,y2)
            match p with
                | Some points ->
                    let p' = Seq.map (fun x -> x,x) points |> Seq.toList |> set |> Set.toList
                    buildMap map (p'@xs)
                | None -> failwith <| sprintf "whaaat: %A" ((x1,y1),(x2,y2))
        | x -> failwith <| sprintf "wat: %A" x


let scanLines =
    data
    |> Seq.pairwise |> Seq.toList
    |> (fun xs -> (Seq.head data,Seq.last data)::xs)
    |> buildMap Map.empty


let isInBounds scanLines (y, (x1,x2)) =
    match Map.tryFind y scanLines with
        | None -> false
        | Some ranges ->
            List.exists (fun (l,h) -> l <= x1 && l <= x2 && x1 <= h && x2 <= h) ranges


let isValidRectangle (inBoundsFun : (int64 * (int64*int64) -> bool)) ((ax,ay), (bx,by)) =
    let tl = min ax bx, min ay by
    let tr = max ax bx, min ay by
    let bl = min ax bx, max ay by
    let br = max ax bx, max ay by
    let ly = snd tl
    let hy = snd bl
    let lx = fst tl
    let hx = fst tr
    let pairsToCheck = [for y in ly .. hy do yield y,(lx,hx)]
    List.forall inBoundsFun pairsToCheck



let folder isValid acc x =
    if fst x = snd x then acc
    else
        let d = dist x
        if d > acc && isValid x then d
        else acc

let inboundsFun = isInBounds scanLines
let isValid = isValidRectangle inboundsFun

Seq.allPairs data data
|> Seq.sortByDescending dist
|> Seq.fold (folder isValid) 0L

