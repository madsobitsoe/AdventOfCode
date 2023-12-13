open System.IO

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

let explode (s:string) : char list = [for c in s do yield c]

let readFile file =
    File.ReadAllText file
    |> (fun s -> s.Split "\n\n")
    |> Array.map (fun s -> s.Split "\n")
    |> Array.map (Array.filter ((<>) ""))
    |> List.ofArray
    |> List.map List.ofArray
    |> List.map (List.map explode)
    |> List.map (List.mapi (fun i x -> List.mapi (fun j y -> (i+1,j+1),y) x))

//let data = readFile "test.txt"
let data = readFile "input.txt"

let rec findMirror f xs =
    match xs with
        | x::y::xs ->
            if List.map snd x = List.map snd y then
                let m1 = List.head x |> fst
                let m2 = List.head y |> fst
                Some (f m1,f m2) :: findMirror f (y::xs)
            else
                findMirror f (y::xs)
        | _ -> [None]


let rec checkMirror (xs : ((int * int) * char) list list) (coords : int * int) =
    let a,b = coords
    let len = List.length xs
    let xs' = List.map (List.map snd) xs
    let toTake = min (len - a) a
    let preskipper = List.skip (a - toTake)
    let xl,xr = preskipper xs' |> List.take toTake |> List.rev, preskipper xs' |> List.skip toTake |> List.take toTake
    if xl = xr then Some coords
    else None



let mapper f x =
    x
    |> findMirror f
    |> List.filter Option.isSome
    |> List.map Option.get
    |> List.map (checkMirror x)
    |> List.filter Option.isSome
    |> List.map Option.get


let mapperHoriz x = mapper fst x 
let mapperVert x =
    List.transpose x
    |> mapper snd


let solve data =
    let horiz = List.collect mapperHoriz data |> List.fold (fun acc x -> acc + (100 * fst x)) 0
    let vert = List.collect mapperVert data |> List.map fst |> List.sum
    horiz + vert

solve data
|> printfn "Part 1: %d"


let flip = function
    | '#' -> '.'
    | '.' -> '#'
    | _ -> failwith "Unexpected input!"

let flipper i j xs =
    List.map (List.map (fun ((i',j'),x) -> if i = i' && j = j' then (i',j'),flip x else (i',j'),x)) xs

// Part 2 fuck me
let rec flipSmudgeAndCheck i j xs =
    
    let ilen = List.length xs
    let jlen = (List.head >> List.length) xs
    if j > jlen then flipSmudgeAndCheck (i+1) 1 xs
    else if i > ilen then 0
    else
        let xs' = flipper i j xs
        match mapperHoriz xs' with
            | x::[] ->
                let rs' = mapperHoriz xs
                if List.isEmpty rs' then 100 * fst x
                else if List.contains x rs' then
                    match mapperVert xs' with
                    | x::[] ->
                        let rs' = mapperVert xs
                        if List.isEmpty rs' then fst x
                        else if List.contains x rs' then flipSmudgeAndCheck i (j+1) xs
                        else fst x
                    | [] -> flipSmudgeAndCheck i (j+1) xs
                    | rs ->
                        let rs' = mapperVert xs
                        List.filter (fun x -> not (List.contains x rs')) rs
                        |> List.head |> fst 

                else 100 * fst x
            | [] ->
                match mapperVert xs' with
                    | x::[] ->
                        let rs' = mapperVert xs
                        if List.isEmpty rs' then fst x
                        else if List.contains x rs' then flipSmudgeAndCheck i (j+1) xs
                        else fst x
                    | [] -> flipSmudgeAndCheck i (j+1) xs
                    | rs ->
                        let rs' = mapperVert xs
                        List.filter (fun x -> not (List.contains x rs')) rs
                        |> List.head |> fst 
            | rs ->
                let rs' = mapperHoriz xs
                let picked =
                    List.filter (fun x -> not (List.contains x rs')) rs
                    |> List.head
                picked |> fst |> (*) 100



List.map (flipSmudgeAndCheck 1 1) data
|> List.sum
|> printfn "Part 2: %d"

