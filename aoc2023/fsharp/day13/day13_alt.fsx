open System.IO

let explode (s:string) : char list = [for c in s do yield c]

let readFile file =
    File.ReadAllText file
    |> (fun s -> s.Split "\n\n")
    |> Array.map (fun s -> s.Split "\n")
    |> Array.map (Array.filter ((<>) ""))
    |> List.ofArray
    |> List.map List.ofArray
    |> List.map (List.map explode)

//let data = readFile "test.txt"
let data = readFile "input.txt"

let toBit = function
    | '.' -> 0
    | '#' -> 1
    | c -> failwith <| sprintf "Unexpected input: %c" c
    
let toBin' x acc = acc <<< 1 ||| toBit x
let toBin = (fun x -> List.foldBack toBin' x  0)
let toBins = List.map toBin
let toBinsVert : char list list -> int list = List.transpose >> toBins

let rec findMirror (ls,rs) =
    printfn "ls: %A\nrs: %A" ls rs
    match ls,rs with
        | [],_ -> None
        | _,[] -> None
        | l::ls',r::rs' ->
            let ls'' = List.skip (List.length ls - List.length rs) ls
            List.map2 (fun a b -> a ^^^ b) ls'' rs |> printfn "%A"
            if l <> r then findMirror (ls, rs')
            else
                printfn "ls: %A\nrs: %A" ls rs
                List.length rs |> Some

let f x =
    x
    |> List.pairwise
    |> List.mapi (fun i x -> i+1,x)
    |> List.filter (fun (i,(x,y)) -> x=y)
    |> List.map fst

let checkMirror x i =
    let toTake = min ((List.length x) - i) i
    let preskipper = List.skip (i - toTake)
    let l = preskipper x |> List.take toTake |> List.rev
    let r = preskipper x |> List.skip toTake |> List.take toTake
    l = r
    
let find fx x =
    let x' = fx x
    let coords = f x'
    List.filter (checkMirror x') coords
    
let solve data =
    let horiz = data |> List.collect (find toBins) |> List.sum
    let vert = data |> List.collect (find toBinsVert) |> List.sum
    horiz * 100 + vert

solve data
|> printfn "part 1: %d"


