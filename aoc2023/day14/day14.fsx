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


let sameInterval xs =
    let xs' = List.tail xs
    if List.isEmpty xs' then false
    else
        xs'
        |> List.pairwise
        |> List.map (fun (a,b) -> a - b)
        |> List.pairwise
        |> List.forall (fun (a,b) -> a - b = 0L)

let findInterval (xs:int64 list) =
    xs
    |> List.pairwise
    |> List.map (fun (a,b) -> b - a)
    |> List.head

let findCycle xs =
    let x = List.head xs
    let start = snd x |> List.head |> fst
    let stop = snd x |> List.skip 1 |> List.head |> fst
    int64 start, int64 stop


let sample = repeat 50000L 0L data [] |> List.mapi (fun i x -> i+1,x)

List.groupBy snd sample
|> List.sortByDescending (fun (a,b) -> List.length b)
|> List.filter (fun (load, x) -> sameInterval (List.map (fst >> int64) x))
|> List.sortByDescending (fun (load, x) -> findInterval (List.map (fst >> int64) x))
|> findCycle
|> (fun (start,stop) -> 1000000000L % (stop - start))
|> (fun i -> List.item ((int i) - 1) sample)
|> snd
|> printfn "Part 2: %A"

