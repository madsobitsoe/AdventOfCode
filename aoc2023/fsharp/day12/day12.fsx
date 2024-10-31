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
    File.ReadAllLines file
    |> Array.map (fun s -> s.Split " ")
    |> Array.map (fun x -> Array.head x, (Array.tail >> Array.head) x)
    |> Array.map (fun (a,b) -> explode a, b.Split "," |> List.ofArray |> List.map int)
    |> List.ofArray


//let data = readFile "test.txt"
let data = readFile "input.txt"


let rec joinWithQuestion xs =
    match xs with
        | [] -> []
        | x::[] -> x
        | x::xs -> x @ '?':: joinWithQuestion xs

let fixData (a,b) =
    (List.replicate 5 >> joinWithQuestion) a, (List.replicate 5 >> List.concat) b

let rec chomp' f n (xs:char list) : char list option =
    match n with
        | 0 ->
            match xs with
                | [] -> Some xs
                | '#'::_ -> None
                | '?'::xs -> Some ('.'::xs)
                | '.'::xs -> Some xs
                | _ -> failwith "Unexpected input"

        | n ->
            match xs with
                | [] -> None
                | '#'::xs' -> f (n-1) xs'
                | '.'::_ -> None
                | '?'::xs' -> f (n-1) xs'
                | _ -> failwith "Unexpected input"


let chomp = memoize chomp'

let rec chomper' chompFun f  key xs =
    match xs with
        | [] -> [None]
        | '.'::xs' -> f key xs'
        | '#'::_ -> [chompFun key xs]
        | '?'::xs' ->
            let a = chompFun key ('#'::xs')
            let b = f key xs'
            a :: b
        | _ -> failwith "Unexpected input"

let chomper = memoize (chomper' chomp)

let rec superchomp chomperFun f  keys xs : int64 =
    if List.length xs < List.sum keys then 0L
    else
    match keys with
        | k::ks ->
            let a = chomperFun k xs
            let a' = List.filter Option.isSome a |> List.map Option.get
            List.fold (fun acc x -> acc + f ks x) 0L a'
        | [] ->
            if List.forall ((=) '.') xs
            then 1L
            else if List.contains ('#') xs
            then 0L
            else 1L
            
let gigachomp = memoize (superchomp chomper)

let solver df chompFun acc x =
    let xs,ks = df x
    let res = chompFun ks xs
    acc + res


let part1Solver = solver id gigachomp
let part2Solver = solver fixData gigachomp

data
|> List.fold part1Solver 0L
|> printfn "Part 1: %A"

data
|> List.fold part2Solver 0L
|> printfn "Part 2: %A"

