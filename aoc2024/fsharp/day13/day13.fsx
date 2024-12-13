open System.IO
open System.Text.RegularExpressions
open System.Numerics

let (|Integer|_|) (str: string) =
   let mutable intvalue = 0I
   if System.Numerics.BigInteger.TryParse(str, &intvalue) then Some(intvalue)
   else None

let getInt = function
    | Integer i -> i
    | x -> failwith <| sprintf "Bad data: %A" x

let (|ParseRegex|_|) regex str =
   let m = Regex(regex).Match(str)
   if m.Success
   then Some (List.tail [ for x in m.Groups -> x.Value ])
   else None

let (|ButtonA|_|) = function
    | ParseRegex "Button A: X\+(\d+), Y\+(\d+)" [Integer a; Integer b] -> Some (a,b)
    | _ -> None

let (|ButtonB|_|) = function
    | ParseRegex "Button B: X\+(\d+), Y\+(\d+)" [Integer a; Integer b] -> Some (a,b)
    | _ -> None


let (|Prize|_|) = function
    | ParseRegex "Prize: X=(\d+), Y=(\d+)" [Integer a; Integer b] -> Some (a,b)
    | _ -> None


let getButtonA = function
    | ButtonA a -> a
    | x -> failwith <| sprintf "Bad data: %A" x

let getButtonB = function
    | ButtonB b -> b
    | x -> failwith <| sprintf "Bad data: %A" x

let getPrize = function
    | Prize p -> p
    | x -> failwith <| sprintf "Bad data: %A" x


let parseMachine xs =
    match xs with
        | a::b::p::_ ->
            let a = getButtonA a
            let b = getButtonB b
            let p = getPrize p
            a,b,p
        | _ -> failwith <| sprintf "Parsing failed: %A" xs
        

let readFile file =
    File.ReadAllText file
    |> (fun s -> s.Split "\n\n")
    |> List.ofArray
    |> List.map (fun s -> s.Split "\n")
    |> List.map List.ofArray
    |> List.map parseMachine

//let data = readFile "test.txt"
let data = readFile "input.txt"



let tupSub (ax:BigInteger,ay:BigInteger) (px,py) =
    (px-ax,py-ay)


let pushA (machine: ((BigInteger*BigInteger)*(BigInteger*BigInteger)*(BigInteger*BigInteger))) =
    let a,b,p = machine
    a,b,tupSub a p

let pushB (machine: ((BigInteger*BigInteger)*(BigInteger*BigInteger)*(BigInteger*BigInteger))) =
    let a,b,p = machine
    a,b,tupSub b p


let part1 (machine: ((BigInteger*BigInteger)*(BigInteger*BigInteger)*(BigInteger*BigInteger))) =
    let allAPushes =
        [0..101]
        |> List.scan (fun acc x -> pushA acc) machine
        |> List.mapi (fun i x -> BigInteger i,x)
        |> List.filter (fun (i,(_,_,(px,py))) -> px >= 0I && py >= 0I)

    let allBPushes =
        allAPushes
        |> List.map (fun (i,m) -> 
                 [0..101]
                 |> List.scan (fun acc x -> pushB acc) m
                 |> List.mapi (fun i' x -> BigInteger i',x)
                 |> List.filter (fun (i',(_,_,(px,py))) -> px = 0I && py = 0I)
                 |> List.map (fun (i',_) -> i,i'))
        |> List.filter ((<>) [])
        |> List.concat
        
    let tokenCost =
        allBPushes
        |> List.map (fun (a:BigInteger,b:BigInteger) -> a * 3I + b)

    match tokenCost with
        | [] -> 0I
        | _ -> List.min tokenCost

data
|> List.map part1
|> List.sum
|> printfn "Stupid solution for part 1: %A"


let part2 (machine: ((BigInteger*BigInteger)*(BigInteger*BigInteger)*(BigInteger*BigInteger))) =
    let (ax,ay),(bx,by),(px,py) = machine

    let i = (ax * by) - (ay*bx)
    let j = (px * by) - (py*bx)
    let k = (ax * py) - (ay * px)
    if j % i <> 0I || k % i <> 0I
    then 0I
    else (3I * j / i) + (k / i)


data
|> List.map part2
|> List.sum
|> printfn "Smart solution for part 1: %A"

data
|> List.map (fun (a,b,(px,py)) -> (a,b,(px+10000000000000I,py+10000000000000I)))
|> List.map part2
|> List.sum
|> printfn "Part 2: %A"
             
