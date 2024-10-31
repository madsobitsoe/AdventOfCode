open System.IO

type Intersect =
    | Parallel
    | PastIntersect of (float*float)
    | InsideIntersect of (float*float)
    | OutsideIntersect of (float*float)

let readFile file =
    File.ReadAllLines file
    |> Array.map (fun s -> s.Split "@")
    |> Array.map (Array.map (fun s -> s.Split ", "))
    |> Array.map (Array.map (Array.map float))
    |> List.ofArray
    |> List.map List.ofArray
    |> List.map (List.map (fun (a:float []) -> a.[0],a.[1],a.[2]))
    |> List.map (fun (x:(float*float*float) list) -> x.[0],x.[1])


//let data' = readFile "test.txt"
let data' = readFile "input.txt"

let rec pairs l = seq {
    match l with
    | h::t -> for e in t do yield h, e
              yield! pairs t
    | _ -> () }
    
    

let get2DVector (x,y,_) = x,y
let get2DVectors (x,v) = get2DVector x, get2DVector v

let p1data =
    data'
    |> List.map get2DVectors
    |> pairs
    |> Seq.toList

let feq f1 f2 = abs (f2 - f1) < 1e-5

let intersect' min max ((l0,v0),(l1,v1)) =
    // printfn "Hailstone A: %A @ %A" l0 v0
    // printfn "Hailstone B: %A @ %A" l1 v1    
    let x0,y0 = l0
    let x1,y1 = fst v0 + x0, snd v0 + y0
    let x2,y2 = l1
    let x3,y3 = fst v1 + x2, snd v1 + y2

    if (x0 = x1 && y0 = y1) || (x2 = x3 && y2 = y3) then
        failwith "length was 0"
    else
        let denominator = (((y3 - y2) * (x1 - x0)) - ((x3 - x2) * (y1 - y0)))
        if feq denominator 0.0 then
            // printfn "Parallel"
            Parallel
        else
            let ua = (((x3 - x2) * (y0 - y2)) - ((y3 - y2) * (x0 - x2))) / denominator
            let ub = (((x1 - x0) * (y0 - y2)) - ((y1 - y0) * (x0 - x2))) / denominator
            let x = x0 + ua * (x1-x0)
            let y = y0 + ua * (y1-y0)
            // printfn "Intersect at %A" (x,y)
            if (ua < 0.0) || (ub < 0.0) then
                PastIntersect (x,y)

            else
                if min <= x && x <= max && min <= y && y <= max then
                    InsideIntersect (x,y)
                else
                    OutsideIntersect (x,y)
            //     printfn "FUTURE"

            // Intersect (x,y)


let intersect = intersect' 200000000000000.0 400000000000000.0

List.map intersect p1data
|> List.filter (fun x -> match x with | InsideIntersect _ -> true | _ -> false)
|> List.length
|> printfn "Part 1: %A"
