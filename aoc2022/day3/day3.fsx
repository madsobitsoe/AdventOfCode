open System.IO
open System.Text.RegularExpressions

let explode (s:string) : string list = [for c in s do yield c] |> List.map string


let readFile file =
    File.ReadAllLines file
    |> List.ofArray
    |> List.map explode


let data = readFile "input.txt"
//let data = readFile "test.txt"

let splitter xs =
    let n = List.length xs / 2
    List.take n xs, List.skip n xs

let findUnique (xs, ys) =
    List.filter (fun x -> List.contains x ys) xs

let transform x =
    let map = ['a'..'z'] @ ['A'..'Z'] |> List.map string
    let mapi = List.indexed map
    List.filter (fun (a,b) -> b = x) mapi |> List.head |> fst |> (+) 1


let part1 data =
    List.map splitter data
    |> List.map findUnique
    |> List.map List.distinct
    |> List.map List.head
    |> List.map transform
    |> List.sum

part1 data
|> printfn "part1: %d"


let rec pick3 (xs:'a list list) =
    match xs with
        
        | [_;_;_] -> [xs]
        | xs -> List.take 3 xs :: pick3 (List.skip 3 xs)

let toTuple xs =
    match xs with
        | [x;y;z] -> x,y,z
        | x -> failwith <| sprintf "oopsie: %A" x


let findUnique2 (xs, ys, zs) =
    List.filter (fun x -> List.contains x ys && List.contains x zs) xs
    |> List.distinct
    |> List.head
    

let part2 data =
    pick3 data
    |> List.map toTuple
    |> List.map findUnique2
    |> List.map transform
    |> List.sum
    
part2 data
|> printfn "part 2: %d"
