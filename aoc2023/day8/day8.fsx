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
    | [] -> failwith "oh heavens no"

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


let allEndsInZ coords =
    List.map explode coords
    |> List.map (List.skip 2 >> List.head)
    |> List.forall (fun x -> x = 'Z')
    
let rec findDest2 map instructions start steps nextInstructions =
    if allEndsInZ start then steps
    // if start = stop then steps
    else
        // printfn "Checking: %A" start
        let inst = List.head nextInstructions
        let start' = List.map (fun x -> findNext x inst map) start
        let next' =
            match List.tail nextInstructions with
            | [] -> instructions
            | xs -> xs
        findDest2 map instructions start' (steps + 1L) next'

findDest2 m inst (findStarts m) 0L inst
|> printfn "Part 2: %d"
