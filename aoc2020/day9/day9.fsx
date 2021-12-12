open System.IO

let readFile filename =
    File.ReadAllLines filename
    |> Array.toList
    |> List.map int64

let test = readFile "test.txt"
let data = readFile "input.txt"

// Part 1
let rec findInvalid (xs : int64 list) n =
    let next26 = List.take 26 xs
    let checkValues = List.take 25 next26 |> List.distinct
    let next = List.skip 25 next26 |> List.head
    let t = List.map (fun x -> next - x) checkValues
    if List.exists (fun x -> List.contains x checkValues) t then findInvalid (List.skip 1 xs) (n-1)
    else next

let part1res = findInvalid data (List.length data)
printfn "Solution part 1: %d" part1res


// part 2
let findContiguousSet (xs:int64 list) goal =
    let xs' = List.filter (fun x -> x < goal) xs
    let rec helper1 xs =
        match xs with
            | [] -> None
            | x::xs ->
                match helper2 (x::xs) [] with
                    | None -> helper1 xs
                    | Some res -> Some res
    and helper2 xs acc =
        let s = List.sum acc
        if s = goal then Some acc
        else if s > goal then None
        else
            match xs with
                | x::xs -> helper2 xs (x::acc)
                | _ -> None
    match helper1 xs' with
        | Some res -> List.min res + List.max res
        | None -> failwith "Did not find a solution"

findContiguousSet data part1res
|> printfn "Solution part 2: %d"
