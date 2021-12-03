open System.IO

// General helper functions
let explode (s:string) = [|for c in s do yield c|]
let implode (cs:char list) = List.map string cs |> List.reduce (+)
let toIntString = List.map (sprintf "%d")
let joinString : (string list -> string) = List.reduce (+)
let toIntAndJoin = toIntString >> joinString
let tupApply f a b = f a , f b
let tupToIntAndJoin = tupApply toIntAndJoin
let tupBitStringToInt = tupApply (fun x -> System.Convert.ToInt32(x,2))

// Just reads the file of binary numbers into a list of ints
let readFile file  =
    File.ReadAllLines file
    |> Array.map explode


let data = readFile "input.txt"
let test = readFile "test.txt"


let leastAndMostCommon (bytes:char [] []) bitIndex =
    Array.fold (fun (z,o) (x: char []) -> if x.[bitIndex] = '0' then (z+1,o) else (z,o+1)) (0,0) bytes
    ||> (fun x y ->  if x > y then (1,0) else (0,1))



// Part 1
let gammaEpsilon (bytes:char [] []) =
    let len = Array.length bytes.[0]
    let rec helper (l,m) = function
        | x when x = len  -> (List.rev l,List.rev m)
        | i ->
            let l',m' = leastAndMostCommon bytes i
            helper (l'::l,m'::m) (i+1)
    helper ([],[]) 0
    ||> tupToIntAndJoin
    ||> tupBitStringToInt

gammaEpsilon data ||> (*) |> printfn "Part 1 solution: %d"

// Part 2
let rec oxygenGen i : (char [] [] -> char []) = function
    | [|x|] -> x
    | bytes ->
        let m = leastAndMostCommon bytes i ||> (fun _ b -> b + 48 |> char)
        Array.filter (fun (x:char[]) -> x.[i] = m) bytes |> oxygenGen (i+1)
let rec oxygenScrub i : (char [] [] -> char []) = function
    | [|x|] -> x
    | bytes ->
        let l = leastAndMostCommon bytes i ||> (fun a _ -> a + 48 |> char)
        Array.filter (fun (x:char[]) -> x.[i] = l) bytes |> oxygenScrub (i+1)

let part2 bytes =
    (oxygenGen 0 bytes, oxygenScrub 0 bytes)
    ||> tupApply (Array.toList)
    ||> tupApply implode
    ||> tupBitStringToInt
    ||> (*)

part2 data |> printfn "Part 2 solution: %d"
