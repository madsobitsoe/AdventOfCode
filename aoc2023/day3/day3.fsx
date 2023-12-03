open System.IO
open System.Text.RegularExpressions

let explode (s:string) : char list = [for c in s do yield c]
let isDigit (s:string) = System.Text.RegularExpressions.Regex("[0-9]").IsMatch s

let getNeighbors max (x,y) =
    [ x-1,y-1;
      x-1,y;
      x-1,y+1;
      x,y-1;
      x,y+1;
      x+1,y-1;
      x+1,y;
      x+1,y+1;
    ]
    |> List.filter (fun (x,y) -> x >= 0 && y >= 0 && x <= max && y <= max)


let readFile file =
    File.ReadAllLines file
    |> Array.map explode
    |> Array.map Array.ofList

// let data = readFile "test.txt"
let data = readFile "input.txt"
let symbols = ['*';'+';'$';'#';'%';'/';'&';'=';'@';'-']

let getSymbolCoords =
    Array.mapi (fun i x ->
                Array.mapi (fun j y -> if List.contains y symbols then Some (i,j) else None) x)
    >> List.ofArray
    >> List.map List.ofArray
    >> List.concat
    >> List.filter (fun x -> match x with None -> false | Some _ -> true)
    >> List.map (fun x -> match x with Some e -> e | None -> failwith "very unexpected!")

let symbolCoords = getSymbolCoords data

let neighborSquares =
    List.map (getNeighbors <| Array.length data.[0]) symbolCoords
    |> List.concat
    |> List.filter (fun (i,j) -> isDigit (string data.[i].[j]))

let rec search depth (i,j) (data : char [] []) neighborSquares =
    if depth <= 0 then false
    else if (i < 0 || i >= Array.length data || j < 0 || j >= Array.length data) then false
    else
        if List.contains (i,j) neighborSquares then true
        else if not (isDigit (string data.[i].[j])) then false
        else
            let right = search (depth - 1) (i,j+1) data neighborSquares
            if right then true
            else
                search (depth - 1) (i,j-1) data neighborSquares
                

let parts =
    Array.mapi (fun i x ->
                Array.mapi (fun j y -> if search 4 (i,j) data neighborSquares
                                       then Some y
                                       else None) x) data


Array.concat parts
|> List.ofArray
|> List.map (fun x -> match x with | Some x -> string x | None -> ",") |> List.reduce (+)
|> (fun s -> s.Split ",")
|> List.ofArray
|> List.filter (fun x -> x <> "")
|> List.map int
|> List.sum
|> printfn "Part 1: %d"


// Part 2

let gear = '*'
let getGearCoords =
    Array.mapi (fun i x ->
                Array.mapi (fun j y -> if y = gear then Some (i,j) else None) x)
    >> List.ofArray
    >> List.map List.ofArray
    >> List.concat
    >> List.filter (fun x -> match x with None -> false | Some _ -> true)
    >> List.map (fun x -> match x with Some e -> e | None -> failwith "Very unexpected!")

let gearCoords = getGearCoords data
let gearNeighborSquares =
    List.map (fun (i,j) -> ((i,j),(getNeighbors (Array.length data.[0])) (i,j))) gearCoords
    |> List.map (fun (l,x) -> l,List.filter (fun (i,j) -> isDigit (string data.[i].[j])) x)
    |> List.filter (fun (l,x) -> List.length x > 1)


let rec findStart (data : char [] []) (i,j) =
    if (i < 0 || j < 0 || i >= Array.length data || j >= Array.length data) then None
    else
        let x = string data.[i].[j]
        if not <| isDigit x then None
        else
            match findStart data (i,j-1) with
                | None -> Some (i,j)
                | start -> start


let rec buildNumber (data : char [] []) (i,j) =
    if (i < 0 || j < 0 || i >= Array.length data || j >= Array.length data) then None
    else
        let x = string data.[i].[j]
        if not <| isDigit x then None
        else
            match buildNumber data (i,j+1) with
                | None -> Some x
                | Some n -> Some (x + n)
    

List.map (fun (g,ns) -> g,List.map (findStart data) ns |> List.distinct |> List.map Option.get) gearNeighborSquares
|> List.filter (fun (_,l) -> List.length l = 2)
|> List.map (fun (g,s) -> g,List.map (buildNumber data) s)
|> List.map (fun (g,r) -> g, List.map (Option.get >> int) r |> List.reduce (*))
|> List.map (fun (_,r) -> r)
|> List.sum
|> printfn "Part 2: %d"

