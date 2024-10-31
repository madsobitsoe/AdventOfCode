open System.IO
open System.Text.RegularExpressions

let (|Long|_|) (str: string) =
    let mutable longValue = 0L
    if System.Int64.TryParse(str, &longValue) then Some longValue
    else None

let (|ParseRegex|_|) regex str =
   let m = Regex(regex).Match(str)
   if m.Success
   then Some (List.tail [ for x in m.Groups -> x.Value ])
   else None

let (|Mapping|_|) = function
    | ParseRegex "(\d+) (\d+) (\d+)" [Long destination; Long source; Long size] -> Some (destination, source, size)
    | _ -> None

let getMapping = function
    | Mapping m -> m
    | _ -> failwith "bad data"

let rec translate map (d:int64)  =
    match map with
        | [] -> d
        | (dest,src,range)::xs ->
            if src <= d && d <= src+range-1L then dest + (d-src)
            else translate xs d 

let readFile file =
    File.ReadAllText file
    |> (fun s -> s.Split "\n\n")
    |> Array.map (fun s -> s.Split "\n")
    |> Array.map List.ofArray
    |> List.ofArray
    |> List.map (List.filter ((<>) ""))

//let data = readFile "test.txt"
let data = readFile "input.txt"

let seeds =
    List.head data |> List.head
    |> (fun s -> s.Split " ")
    |> Array.tail
    |> Array.map int64
    |> List.ofArray


let genMap l =
    List.tail l
    |> List.map getMapping

let genMaps data =
    List.tail data
    |> List.map genMap

let maps = genMaps data |> List.map translate

let finalMap = List.reduce (>>) maps

List.map finalMap seeds
|> List.min
|> printfn "Part 1: %d"



// Part 2, seeds are now ranges goddamnit
let rec listToTuples xs : (int64 * int64) list =
    match xs with
        | [] -> []
        | x::y::xs -> (x,x+y-1L):: listToTuples xs
        | _ -> failwith "oh heavens no"

let seedsAsRanges =
    List.head data |> List.head
    |> (fun s -> s.Split " ")
    |> Array.tail
    |> Array.map int64
    |> List.ofArray
    |> listToTuples
    |> List.map Some


let overlap (a:int64,b:int64) (x:int64,y:int64) =
    if b < x || a > y then None
    else if a <= x && b >= y then Some (x,y)
    else if a >= x && b <= y then Some (a,b)
    else if a <= x && b <= y then Some (x,b)
    else Some (a,y)

let splitRange (a:int64,b:int64) (x:int64,y:int64) =
    let below =
        if a >= x then None
        else if b < x then Some (a,b)
        else Some (a, x-1L)
    let above =
        if b <= y then None
        else if a > y then Some (a,b)
        else Some (y+1L,b)
    let overlap = overlap (a,b) (x,y)
    below,overlap,above
        

let translateRange' (destination:int64,source:int64,range:int64) (x:(int64 * int64) option) : ((int64*int64) option * (int64*int64) option list) =
    if Option.isNone x then None,[]
    else
        let start,stop = Option.get x
        let sourceRange = source, source + range - 1L
        let below,translatable,above = splitRange (start,stop) sourceRange
        let translator x = destination + (x - source)
        let translated = Option.map (fun (a,b) -> translator a, translator b) translatable
        translated,[below;above]




let folder map (translated:(int64*int64) option list,rest:(int64*int64) option list) x =
    let translated', rest' = translateRange' map x
    translated'::translated,rest'@rest
    
let rec translateRange map (translatedRanges:(int64*int64) option list) (rest:(int64*int64) option list) =
    match map with
        | [] -> translatedRanges @ rest
        | m::xs ->            
           let translated,untranslated = List.fold (folder m) ([],[]) rest
           translateRange xs (translated@translatedRanges) (untranslated)

let rangeMap =
    genMaps data
    |> List.map (fun x -> translateRange x [])
    |> List.reduce (>>)


rangeMap seedsAsRanges
|> List.filter Option.isSome
|> List.map Option.get
|> List.minBy fst
|> fst
|> printfn "Part 2: %A"
