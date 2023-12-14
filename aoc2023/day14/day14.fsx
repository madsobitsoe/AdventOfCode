open System.IO

let explode (s:string) : char list = [for c in s do yield c]

type Direction =
    | North
    | South
    | West
    | East
    
let memoize f =
    let d = new System.Collections.Generic.Dictionary<_,_>()
    let rec g x y =
        match d.TryGetValue ((x,y)) with
            | (true, res) -> res
            | _ ->
                let res = f g x y
                d.Add((x,y), res)
                res
    g

let memoizeSingle f =
    let d = new System.Collections.Generic.Dictionary<_,_>()
    let rec g x =
        match d.TryGetValue (x) with
            | (true, res) -> res
            | _ ->
                let res = f g x
                d.Add(x, res)
                res
    g

let readFile file =
    File.ReadAllLines file
    |> Array.map explode
    |> List.ofArray

//let data = readFile "test.txt"
let data = readFile "input.txt"

let rec slideNorth' f xs =
    match xs with
        | 'O'::xs -> 'O'::f xs
        | '#'::xs -> '#'::f xs
        | '.'::'O'::xs -> 'O'::f ('.'::xs)
        | '.'::'#'::xs -> '.'::'#'::f xs
        | '.'::'.'::xs' ->
            let x = f ('.'::xs')
            '.'::x
        | c::[] -> xs
        | [] -> []

let slideNorth = memoizeSingle slideNorth'

let rec fullSlide' f xs =
    let xs' = slideNorth xs
    if xs' = xs then xs
    else f xs'

let fullSlide = memoizeSingle fullSlide'

let rec slide' f dirs xs =
    match dirs with
        | [] -> xs
        | North::dirs' ->
            List.transpose xs |> List.map fullSlide |> List.transpose |> f dirs'
        | South::dirs' ->
            List.rev xs |> List.transpose |> List.map fullSlide |> List.transpose |> List.rev |> f dirs'
        | West::dirs' -> List.map fullSlide xs |> f dirs'
        | East::dirs' -> List.map List.rev xs |> List.map fullSlide |> List.map List.rev |> f dirs'

let slide = memoize slide'

let rec cycle' f xs =
    slide [North; West; South; East] xs

let cycle = memoizeSingle cycle'

let calculateLoad xs =
    xs
    |> List.rev
    |> List.mapi (fun i x -> (x |> List.filter ((=) 'O') |> List.length |> int64) * ((int64 i) + 1L))
    |> List.sum

let rec repeat times n xs acc =
    if times = n then acc |> List.rev
    else
        let xs' = cycle xs
        let load = calculateLoad xs'
        repeat times (n+1L) xs' (load::acc)


List.transpose data
|> List.map fullSlide
|> List.transpose
|> List.rev
|> List.mapi (fun i x -> (x |> List.filter ((=) 'O') |> List.length) * (i+1))
|> List.sum
|> printfn "Part 1: %A"


let rec detectCycle skip n data =
    if skip = 0 then printfn "Looking for cycle of size %d" n
    let xs = List.skip skip data
    if skip = n-1 then None
    else if List.length xs < n*n then None
    else
    match xs with
        | [] -> None
        | _ ->
            let h = List.take n xs
            let h' = List.map snd h
            let t =
                List.fold (fun (acc,cs) _ -> (List.take n cs)::acc, List.skip n cs) ([],xs) [1..n]
                |> fst
                |> List.map (List.map snd)
            if List.forall ((=) h') t then
                printfn "Found cycle:\n%A" h
                Some h
            else
                match detectCycle (skip + 1) n data with
                    | None -> detectCycle 0 (n+1) data
                    | x -> x


let sample = repeat 25000L 0L data [] |> List.mapi (fun i x -> i+1,x)

sample
|> detectCycle 0 153
|> (fun x -> Option.get x)
|> (fun x -> List.head x |> fst |> int64, List.last x |> fst |> int64)
|> (fun (start,stop) -> printfn "start:%A, stop: %A" start stop; ((1000000000L) % stop + stop ))
|> (fun i -> List.item (int i) sample)
|> snd
|> printfn "Part 2: %A"

