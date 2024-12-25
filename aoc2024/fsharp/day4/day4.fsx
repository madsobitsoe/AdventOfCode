#time
open System.IO
open System.Text.RegularExpressions

let explode (s:string) = [for c in s do yield c] |> Array.ofList

let readFile file  =
    File.ReadAllText file
    |> (fun s -> s.Split "\n")
    |> Array.map explode 
    |> Array.filter (fun x -> x <> [||])


let to2dArray xs =
    let l1 = List.length xs
    let l2 = List.length (List.head xs)
    let init = (fun i j -> List.item j (List.item i xs ))
    Array2D.init l1 l2 init

let data = readFile "input.txt"
//let data = readFile "test.txt"


let rec countXmas acc a =
    if List.length a < 4 then acc
    else
    match a with
        | 'X'::'M'::'A'::'S'::_ -> countXmas (acc+1) (List.tail a)
        | 'S'::'A'::'M'::'X'::_ -> countXmas (acc+1) (List.tail a)
        | _::rest -> countXmas acc rest


let getDiags i j =
    ((i,j),(i+1,j+1),(i+2,j+2),(i+3,j+3)),((i,j),(i+1,j-1),(i+2,j-2),(i+3,j-3))


let isXmas (xs: char array2d) coords =
    let (c1x,c1y),(c2x,c2y),(c3x,c3y),(c4x,c4y) = coords
    let c1 = xs.[c1x,c1y]
    let c2 = xs.[c2x,c2y]
    let c3 = xs.[c3x,c3y]
    let c4 = xs.[c4x,c4y]
    match (c1,c2,c3,c4) with
        | ('X','M','A','S')
        | ('S','A','M','X') -> true
        | _ -> false

let countDiagonals xs =
    let asArr = to2dArray xs
    let mutable sum = 0
    for i = 0 to (Array2D.length1 asArr) - 4 do
        for j = 0 to (Array2D.length2 asArr) - 1 do
            let diag1,diag2 = getDiags i j 
            let down = if j <= (Array2D.length2 asArr) - 4 && isXmas asArr diag1 then 1 else 0
            let up = if j >= 3 && isXmas asArr diag2 then 1 else 0
            sum <- sum + down + up
    sum

let countAll xs =
    let horiz = List.map (countXmas 0) xs |> List.sum
    let vert = List.transpose xs |> List.map (countXmas 0) |> List.sum
    let diags = countDiagonals xs
    horiz + vert + diags

let part1 data =
    let xs = Array.map List.ofArray data |> List.ofArray
    countAll xs

part1 data
|> printfn "Part 1: %d"


let isXmas2 (xs : char array2d) i j =
    if i < 1 || j < 1 || i > (Array2D.length1 xs) - 2 || j > (Array2D.length2 xs) - 2 then false
    else
        let c = xs.[i,j]
        if c <> 'A' then false
        else
            let c1 = xs.[i-1,j-1]
            let c2 = xs.[i+1,j+1]
            let c3 = xs.[i-1,j+1]
            let c4 = xs.[i+1,j-1]
            let t1 =
                match (c1,c2) with
                | ('M','S')
                | ('S','M') -> true
                | _ -> false
            let t2 =
                match (c3,c4) with
                | ('M','S')
                | ('S','M') -> true
                | _ -> false
            t1 && t2

let part2 (data : char array2d) =    
    let mutable sum = 0
    for i = 1 to Array2D.length1 data - 2 do
        for j = 1 to Array2D.length2 data - 2 do
            if isXmas2 data i j then
                sum <- sum + 1
    sum

data
|> Array.map List.ofArray
|> List.ofArray
|> to2dArray
|> part2 
|> printfn "Part 2: %d"
