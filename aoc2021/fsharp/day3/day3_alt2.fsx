open System.IO

let explode (s:string) = [for c in s do yield c]
let implode (cs:char list) = List.map string cs |> List.rev |> List.fold (+) ""
let cToInt (c:char) = int c - 48

// Transpose the array and we have each bit-index of all bytes as each row
let readFile file  =
    File.ReadAllLines file
    |> List.ofArray
    // |> List.map (explode >> List.map cToInt)
    |> List.map explode
    |> List.transpose

let rec lm (z,o) = function
    | [] -> (z,o)
    | '0'::xs -> lm (z+1,o) xs
    | '1'::xs -> lm (z,o+1) xs
    | _ -> failwith "Oh boy, baaaad data"

let folder (ll,ml) (z,o) =
    if z > o then ('1'::ll,'0'::ml) else ('0'::ll,'1'::ml)

let genByte xs = List.fold folder ([],[]) xs

let tupApply f (a, b) = f a, f b
let data = readFile "input.txt"
let test = readFile "test.txt"

// Part 1 
List.map (lm (0,0)) test
|> genByte 
|> tupApply (implode >> (fun x -> System.Convert.ToInt32(x,2)))
||> (*)
|> printfn "Part 1 solution: %d"

