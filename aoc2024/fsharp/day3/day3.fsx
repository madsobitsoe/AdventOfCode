#time
open System.IO
open System.Text.RegularExpressions

let explode (s:string) : string list = [for c in s do yield string c] 
let isDigit (s:string) = System.Text.RegularExpressions.Regex("[0-9]").IsMatch s

let (|Integer|_|) (str: string) =
   let mutable intvalue = 0
   if System.Int32.TryParse(str, &intvalue) then Some(intvalue)
   else None

let (|ParseRegex|_|) regex str =
   let m = Regex(regex).Match(str)
   if m.Success
   then Some (List.tail [ for x in m.Groups -> x.Value ])
   else None

let (|Mul|_|) = function
    | ParseRegex "mul\((\d{1,3}),(\d{1,3})\)" [Integer a; Integer b] -> Some (a,b)
    | _ -> None

let getMuls = function
    | Mul m -> m
    | x -> (0,0)

let rec dos acc cs =
    match cs with
        | "d"::"o"::"n"::"'"::"t"::"("::")"::rest -> donts acc rest
        | c::rest -> dos (acc + c) rest
        | _ -> acc

and donts acc cs =
    match cs with
        | "d"::"o"::"("::")"::rest -> dos acc rest
        | c::rest -> donts acc rest
        | _ -> acc


// Parsing is hard and boring
let readFile file =
    File.ReadAllText file

let replace (s:string) =
    s
    |> (fun s -> s.Replace (")", ")\n"))
    |> (fun s -> s.Split "\n")
    
//let data = readFile "test.txt"
let data = readFile "input.txt"

let folder acc (x,y) = acc + x*y

let part1 data =
    data
    |> replace
    |> Array.map getMuls
    |> Array.fold folder 0

part1 data
|> printfn "Part 1: %d"

let part2 data =
    data
    |> explode
    |> dos ""
    |> replace
    |> Array.map getMuls
    |> Array.fold folder 0


part2 data
|> printfn "Part 2: %d"

