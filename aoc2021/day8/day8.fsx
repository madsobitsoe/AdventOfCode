open System.IO

let explode (s:string) : char list = [for c in s do yield c]

let readFile file  =
    File.ReadAllLines file
    |> Array.map (fun (s:string) -> s.Split("|"))
    |> Array.map Array.toList
    |> List.ofArray
    |> List.map (fun x -> List.map (fun (s:string) -> s.Split(" ")) x )
    |> List.map (fun x -> List.map (Array.toList ) x)
    |> List.map (fun x -> List.map (List.filter (fun (s:string) -> s <> "")) x)


let data = readFile "input.txt"
let test = readFile "test.txt"


let rec countSeg acc (xs:string list) =
    match xs with
    | [] -> acc
    | x::xs ->
        let len = x.Length
        if len = 2 || len = 4 || len = 3 || len = 7 then countSeg (acc+1) xs
        else countSeg acc xs
        
let rec countSegs acc xs =
    match xs with
        | [] -> acc
        | x::y::xs -> countSegs (acc + (countSeg 0 y)) xs
        | _ -> failwith "bad data"
            

List.fold countSegs 0 data
|> printfn "Solution part 1: %d"


// 1 == 2 segments
// 4 == 4 segments
// 7 == 3 segments
// 8 = 7 segments



let updateBindings x bindings (pot: Map<char,int list>) =
    let entry = pot.[x]
    if List.length entry <= List.length bindings then pot
    else Map.add x bindings pot
// Find binding for the top segment    
let shrink x (pot: Map<char,int list>) =
    let x' = explode x
    let len = List.length x'
    match len with
        // We know this is the digit 1
        | 2 ->
            // get current potential bindings for each char in x'
            List.fold (fun acc x -> updateBindings x [3;6] acc) pot x'
            // We know this is the digit 7
        | 3 ->
            // find the char that has the largest binding
            // it has to be "the top of the seven", i.e. only binding = 1 is possible
            // List.map (fun x -> (x, pot.[x])) x'
            let topseg = List.maxBy (fun x -> List.length (pot.[x])) x'
            // |> List.map (fun (x,_) -> x)
            updateBindings topseg [1] pot
            
            // We know this is the digit 4
        | 4 ->  pot
            // Two of the chars should have pot bindings of [3;6] or less (from digit 1)
            // The other two will be either coord 2 or 4
            // let binds = List.filter (fun x -> List.length (pot.[x]) > 2)  x'
            // List.fold (fun acc x -> updateBindings x [2;4] acc) pot binds
            // We know this is 2, 3 or 5
        | 5 ->  pot
            // we know this is 0, 6 or 9
        | 6 -> pot
        // We know this is 8 (but uses all, so fuck me
        | 7 -> pot
        //     // get current potential bindings for each char in x'
        //     List.fold (fun acc x -> updateBindings x [2;3;4;6] acc) pot x'
            
        | _ -> failwith "nope"


// Find bindings for segments b, c, e and f
let appears c xs = List.contains c xs
let appearances xs =
    // For each char
    // Count the occurence of it in each of the strings
    let chars = ['a'..'g']
    let xs' = List.map explode xs
    List.map (fun c -> List.filter (List.contains c) xs' |> (fun x -> (c,List.length x))) chars

// segment-occurences in digit strings
// f = 9
// a = 8
// c = 8
// d = 7
// g = 7
// b = 6
// e = 4
let appearancesToMap (c,appears) (pot:Map<char,int list>) =
    match appears with
        | 4 -> Map.add c [5] pot
        | 6 -> Map.add c [2] pot
        | 7 ->
            if List.length (pot.[c]) > 2 then 
                Map.add c [4;7] pot
            else pot
        | 8 ->
            // Check if this is the top piece
            // if not, it is the third
            if List.length (pot.[c]) = 1 then pot
            else Map.add c [3] pot
        | 9 -> Map.add c [6] pot
        | _ -> pot

// Find binding for segment d
// Take the input list
// segment d is the char in the 4-char string, that does not have a binding
let findD xs (pot:Map<char,int list>) =
    let xs' = List.map explode xs
    let binds = List.filter (fun x -> List.length x = 4) xs' |> List.head
    List.fold (fun (acc:Map<char,int list>) (x:char) -> if List.length (acc.[x]) > 1 then Map.add x [4] acc else acc) pot binds
    
// g is the only char in the map without a binding
let findG (pot: Map<char,int list>) =
    let elm = Map.toList pot |> List.filter (fun (c,b) -> List.length b > 1) |> List.map fst |> List.head
    Map.add elm [7] pot


let getBindings inputData =
    let indices = [1..8]
    let potentials  =
        Map.ofList [('a', indices); ('b', indices); ('c', indices); ('d', indices); ('e', indices); ('f', indices); ('g', indices)]
    match inputData with
        | x::_::_ ->
            let step1 =
                List.sortBy (fun (s:string) -> s.Length) x
                |> List.fold (fun acc x -> shrink x acc) potentials
            let step2 =
                appearances x
                |> List.fold (fun acc x -> appearancesToMap x acc) step1
            let step3 =
                findD x step2
            let step4 = findG step3 |> Map.map (fun k v -> List.head v)
            step4
        | _ -> failwith "error!"
            

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

    
let entryToValue (bindings:Map<char,int>) x =
    let entry = explode x
    List.map (fun x -> bindings.[x]) entry


// For each entry, get bindings
// Then map the entry to a digit with those bindings
let solveEntry (x:string list list) =
    let bindings = getBindings x
    match x with
        | x::y::_ ->
            List.map (entryToValue bindings) y
            |> List.map toDigit
            |> List.foldBack (fun c acc -> string c + acc) <| ""
            |> int
        | _ -> failwith "bad solve entry"


List.map solveEntry data
|> List.sum
|> printfn "Solution part 2: %d"
