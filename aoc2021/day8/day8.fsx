open System.IO
open System.Text.RegularExpressions

let explode (s:string) : char list = [for c in s do yield c]

let (|ParseRegex|_|) regex str =
   let m = Regex(regex).Match(str)
   if m.Success
   then Some (List.tail [ for x in m.Groups -> x.Value ])
   else None

let (|StringList|_|) (str:string) =
    str.Split(" ") |> Array.filter (fun s -> s <> "") |> Array.toList |> Some

let (|Entry|_|) = function
    | ParseRegex "((?:[a-z]+\W*){10})\|((?:\W*[a-z]+){4})" [StringList ls; StringList rs] -> Some (ls, rs)    
    | _ -> None

let parseEntry = function
    | Entry entry -> entry
    | _ -> failwith "No valid entry found"

// Get a list of tuples, (string list * string list)
let readFile filename =
    File.ReadAllLines filename
    |> Array.map parseEntry
    |> List.ofArray

let test = readFile "test.txt"    
let data = readFile "input.txt"

// Part 1
let rec countSeg acc : string list -> int = function
    | [] -> acc
    | x::xs ->
        let len = x.Length
        if List.contains len [2;3;4;7] then countSeg (acc+1) xs
        else countSeg acc xs

let rec countSegs acc = function
    | [] -> acc
    | (_,segs)::xs -> countSegs (acc + countSeg 0 segs) xs


countSegs 0 data
|> printfn "Solution part 1: %d"

// Part 2
type bindings = Map<char,int>
// segment-occurences in digit strings:
// f = 9 ,a = 8 ,c = 8 ,d = 7 ,g = 7 ,b = 6 ,e = 4
let appearancesToMap (pot:bindings) (c,appears)  =
    match appears with
        | 4 -> Map.add c 5 pot
        | 6 -> Map.add c 2 pot
        | 8 ->
            // Check if this is the top piece
            // if not, it is the third
            if Map.containsKey c pot then pot 
            else Map.add c 3 pot
        | 9 -> Map.add c 6 pot
        | _ -> pot

let appearances xs =
    // For each char
    // Count the occurence of it in each of the strings
    let chars = ['a'..'g']
    let xs' = List.map explode xs
    List.map (fun c -> List.filter (List.contains c) xs' |> (fun x -> (c,List.length x))) chars

let findD xs (pot:bindings) =
    let xs' = List.map explode xs
    let binds = List.filter (fun x -> List.length x = 4) xs' |> List.head
    List.fold (fun (acc:bindings) (x:char) -> if Map.containsKey x acc then acc else Map.add x 4 acc) pot binds

// segment g is the only char in the map without a binding
let findG (pot: bindings) =
    let chars = ['a'..'g']
    List.fold (fun map k -> if Map.containsKey k map then map else Map.add k 7 map) pot chars


let getBindings ls =
    // First, figure out the top segment, segment 'a'
    let top = List.filter (fun (s:string) -> s.Length <= 3) ls |> List.map explode |> List.concat |> List.countBy id |> List.filter (fun (_,c) -> c = 1) |> Map.ofList 
    // Now we can find segments b,c,e,f based on their occurrence in strings on LHS
    let step2 = appearances ls |> List.fold appearancesToMap top
    // Now we can find segment D, which will be the char in the 4-char string without a binding
    let step3 = findD ls step2
    // And lastly, the char without a binding is segment 'g'
    let step4 = findG step3
    step4

let toDigit xs =
    let x = List.sort xs
    match x with
    | [1;2;3;5;6;7] -> '0'
    | [3;6] -> '1'
    | [1;3;4;5;7] -> '2'
    | [1;3;4;6;7] -> '3'
    | [2;3;4;6] -> '4'
    | [1;2;4;6;7] -> '5'
    | [1;2;4;5;6;7] -> '6'
    | [1;3;6] -> '7'
    | [1;2;3;4;5;6;7] -> '8'
    | [1;2;3;4;6;7] -> '9'
    | _ -> failwith <| sprintf "bad digit! %A" x

let entryToValue (bindings:bindings) x =
    let entry = explode x
    List.map (fun x -> bindings.[x]) entry

let solveEntry (ls,rs) =
    let bindings = getBindings ls
    List.map (entryToValue bindings) rs
    |> List.map toDigit
    |> List.foldBack (fun c acc -> string c + acc) <| ""
    |> int

List.map solveEntry data
|> List.sum
|> printfn "Solution part 2: %d"

