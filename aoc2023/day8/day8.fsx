open System.IO

let explode (s:string) : char list = [for c in s do yield c]

let readFile file =
    File.ReadAllText file
    |> (fun s -> s.Split "\n\n")
    |> (fun x -> Array.head x, (Array.skip 1 >> Array.head) x)


let rec toPairs xs =
    match xs with
    | [x]::[y;z]::[] -> [(x,(y,z))]
    | [x]::[y;z]::xs -> (x,(y,z))::toPairs xs
    | _ -> failwith "oh heavens no"
    

let parseMap (map:string) =
    map.Split "\n"
    |> Array.map (fun s -> s.Split "=")
    |> Array.map (Array.map (fun s -> s.Replace ("(", "")))
    |> Array.map (Array.map (fun s -> s.Replace (")", "")))
    |> Array.map (Array.map (fun s -> s.Split ","))
    |> Array.map (Array.map (Array.map (fun s -> s.Replace (" ", ""))))
    |> Array.concat
    |> Array.map (Array.filter ((<>) ""))
    |> Array.filter (fun x -> Array.length x <> 0)
    |> List.ofArray
    |> List.map List.ofArray
    |> toPairs


let instructions,map =
    readFile "input.txt"    
    |> (fun (i,m) -> explode i, parseMap m)


let rec findNext dest inst xs =
    match xs with
        | (x,(l,r))::_ when dest = x ->
            match inst with
                | 'R' -> r
                | 'L' -> l
                | _ -> failwith <| sprintf "Bad instruction: %A" inst
        | _::xs -> findNext dest inst xs
        | [] -> failwith "oh oh"


let rec findDest map instructions start stop steps nextInstructions =
    if start = stop then steps
    else
        let inst = List.head nextInstructions
        let start' = findNext start inst map
        let next' =
            match List.tail nextInstructions with
            | [] -> instructions
            | xs -> xs
        findDest map instructions start' stop (steps + 1) next'


findDest map instructions "AAA" "ZZZ" 0 instructions
|> printfn "Part 1: %d"


// Part 2 wtf
let inst,m =
    readFile "input.txt"
    |> (fun (i,m) -> explode i, parseMap m)

let findStarts map =
    List.map fst map
    |> List.map explode
    |> List.filter (List.skip 2 >> List.head >> (=) 'A')
    |> List.map (List.map string)
    |> List.map (List.reduce (+))

let endsInLetter c =
    explode >>
    List.skip 2
    >> List.head
    >> (=) c

let endsInZ = endsInLetter 'Z'

let fn m k i =
    let find = match i with | 'L' -> fst | 'R' -> snd | _ -> failwith "wrong inst"
    Map.find k m |> find 

let rec findDest3 findNext instructions start steps nextInstructions =
    let inst = List.head nextInstructions
    let start' = findNext start inst
    let next' =
        match List.tail nextInstructions with
            | [] -> instructions
            | xs -> xs
    if endsInZ start' then start',(steps + 1L)
    else findDest3 findNext instructions start' (steps + 1L) next'


let rec getInstructions inst n =
    let len = List.length inst
    if n < len then List.skip n inst
    else getInstructions inst (n-len)

let rec gcd a b =
    match b with
        | 0L -> a
        | _ -> gcd b (a % b)

let lcm (a:int64) b = a * b / gcd a b

let m' = Map m
let starts = findStarts m


List.map (fun x -> findDest3 (fn m') inst x 0L inst) starts
|> List.map snd
|> List.foldBack lcm <| 1L
|> printfn "Part 2: %A"
