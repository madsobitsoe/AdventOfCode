open System.IO
open System.Text.RegularExpressions

type Instruction = On of (int64 * int64) * (int64 * int64) * (int64 * int64) | Off of (int64 * int64) * (int64 * int64) * (int64 * int64)


let (|ParseRegex|_|) regex str =
   let m = Regex(regex).Match(str)
   if m.Success
   then Some (List.tail [ for x in m.Groups -> x.Value ])
   else None

let (|Entry|_|) = function
    | ParseRegex "on x=([-]?[0-9]+)\.\.([-]?[0-9]+),y=([-]?[0-9]+)\.\.([-]?[0-9]+),z=([-]?[0-9]+)\.\.([-]?[0-9]+)" [x1; x2; y1;y2;z1;z2] -> Some (On ((int64 x1, int64 x2),(int64 y1, int64 y2), (int64 z1, int64 z2)))
    | ParseRegex "off x=([-]?[0-9]+)\.\.([-]?[0-9]+),y=([-]?[0-9]+)\.\.([-]?[0-9]+),z=([-]?[0-9]+)\.\.([-]?[0-9]+)" [x1; x2; y1;y2;z1;z2] -> Some (Off ((int64 x1, int64 x2),(int64 y1, int64 y2), (int64 z1, int64 z2)))    

    | _ -> None

let parseInstruction = function
    | Entry e -> e
    | _ -> failwith "oops"


let readFile filename =
    File.ReadAllLines filename
    |> Array.toList
    |> List.map parseInstruction

// let test = readFile "test.txt"
// let test2 = readFile "test2.txt"
// let test2p1 = readFile "test2p1.txt"
let data = readFile "input.txt"




// For part 1, only consider region (-50..50) for each coord
let performInstructionPart1 (cubes:int64[,,]) inst =
    match inst with
        | On ((x1,x2),(y1,y2),(z1,z2)) ->
            let coords =
                [for x in (max -50L x1) .. (min 50L x2) do
                 for y in (max -50L y1) .. (min 50L y2) do
                 for z in (max z1 -50L) .. (min 50L z2) do yield (x+50L,y+50L,z+50L)]
            List.iter (fun ((x,y,z)) -> cubes.[int x,int y,int z] <- 1L) coords
            // |> Set.fold (fun acc coord -> Set.add coord acc) cubes
        | Off ((x1,x2),(y1,y2),(z1,z2)) ->
            let coords =
                [for x in (max -50L x1) .. (min 50L x2) do
                 for y in (max -50L y1) .. (min 50L y2) do
                 for z in (max -50L z1) .. (min 50L z2) do yield (x+50L,y+50L,z+50L)]
            List.iter (fun  ((x,y,z)) -> cubes.[int x,int y,int z] <- 0L) coords

    

//Part 1
let rec performInstructionsPart1 insts cubes =
    match insts with
    | [] -> cubes
    | i::is ->
        performInstructionPart1 cubes i
        performInstructionsPart1 is cubes

let arr = Array3D.init 101 101 101 (fun x y z -> 0L)

performInstructionsPart1 data arr
let sumArr arr =
    let mutable s = 0L
    for x = 0 to Array3D.length1 arr - 1 do
        for y = 0 to Array3D.length2 arr - 1 do
            for z = 0 to Array3D.length3 arr - 1 do
                s <- s + arr.[x,y,z]
    s
sumArr arr
|> printfn "Solution part 1: %d"

// Part 2

// We need to run the reboot procedure for all cubes in this stupid infinite 3d-grid
// We clearly can't make an array of size a billion
// We already map the input data to regions
// Can we reduce that to regions that will turn on/off?


let span (start:int64) stop =
    abs (max start stop - min start stop) + 1L
        

// So, we are dealing with cuboids of dims X*Y*Z
// When we turn on a range, we get a new cuboid
// When we turn off a range, we need to split current cuboids
let splitCuboid c1 c2 =
    let (x0,x1),(y0,y1),(z0,z1) = c1
    let (x0',x1'),(y0',y1'),(z0',z1') = c2
    // Is c1 fully contained in c2?
    // Then we just remove c1
    if x0' <= x0 && x1 <= x1' && y0' <= y0 && y1 <= y1' && z0' <= z0 && z1 <= z1' then []
    // is c2 to the left or right of c1?
    // Then we keep all of c1 as a single cuboid
    else if x1' < x0 || x1 < x0' then [c1]
    // is c2 above or below c1?
    // Then we keep all of c1 as a single cuboid    
    else if y1' < y0 || y1 < y0' then [c1]
    // is c2 in front of or behind c1?
    // Then we keep all of c1 as a single cuboid    
    else if z1' < z0 || z1 < z0' then [c1]
    
    // Well, we have an overlap
    // Split c1 into smaller cubes
    else
        // We need l,r,u,d,f,b (6)
        // as well as lf,lb,rf,rb (4)
        // and luc,luf,lub,ldc,ldf,ldb (6)
        // and ruc,ruf,rub,rdc,rdf,rdb (6)
        // For a total maximum of 26 cubes (dear lord)
        // Instead we do
        // Full left side (l,lf,lb, luc,luf,lub,ldc,ldf,ldb) -> 9 pieces in one
        // Full right side (r,rf,rb, ruc,ruf,rub,rdc,rdf,rdb) -> 9 pieces in one
        // full bottom minus l/r-sides (d,df,db) - 3 pieces
        // Full top minus l/r-sides (u,uf,ub) -> 3 pieces
        // f by itself -> 1 piece
        // b by itself -> 1 piece
        let nc1 =
            seq {
                    // Full left
                    if x0 < x0' then yield (x0,x0'-1L),(y0,y1),(z0,z1)
                    // Full right
                    if x1 > x1' then yield (x1'+1L,x1),(y0,y1),(z0,z1)
                    // bottom without l/r
                    if y0 < y0' then yield (max x0 x0',min x1 x1'),(y0,y0'-1L),(z0,z1)
                    // Top without l/r
                    if y1' < y1 then yield (max x0 x0',min x1 x1'),(y1'+1L,y1),(z0,z1)
                    // Forward
                    if z0 < z0' then yield (max x0 x0',min x1 x1'),(max y0 y0',min y1 y1'),(z0,z0'-1L)
                    // Backward
                    if z1' < z1 then yield (max x0 x0',min x1 x1'),(max y0 y0',min y1 y1'),(z1'+1L,z1)
                }
        nc1 |> Seq.toList

let joinCuboids c1 c2 =
    let (x0,x1),(y0,y1),(z0,z1) = c1
    let (x0',x1'),(y0',y1'),(z0',z1') = c2
    // Is c2 fully contained in c1?
    if x0 <= x0' && x1' <= x1 && y0 <= y0' && y1' <= y1 && z0 <= z0' && z1' <= z1 then [c1]
    // Is c1 fully contained in c2?
    else if x0' <= x0 && x1 <= x1' && y0' <= y0 && y1 <= y1' && z0' <= z0 && z1 <= z1' then [c2]
    // is c2 to the left or right of c1?
    else if x1' < x0 || x1 < x0' then [c1;c2]
    // is c2 above or below c1?
    else if y1' < y0 || y1 < y0' then [c1;c2]
    // is c2 in front of or behind c1?
    else if z1' < z0 || z1 < z0' then [c1;c2]
    // Well, we have an overlap
    // keep c2
    // Split c1 into smaller cubes
    else        
        let nc1 = splitCuboid c1 c2
        c2::(nc1 |> Seq.toList)


let countCuboid ((x0,x1),(y0,y1),(z0,z1)) = span x0 x1 * span y0 y1 * span z0 z1
let countCuboids cs = List.fold (fun acc x -> countCuboid x + acc) 0L cs


// When performing a new instruction,

// we need to go through all cuboids and split them based on the new one
// if it's an on-instruction, we add the new cuboid
// if it's an off-instruction, we just split and don't add
let rec performInstructions acc insts =
    match insts with
        | [] -> countCuboids acc
        | c::cs ->
            match c with
                | On (x,y,z) ->
                    let acc' = (x,y,z)::List.collect (fun c' -> splitCuboid c' (x,y,z)) acc
                    performInstructions acc' cs
                | Off (x,y,z) ->
                    let acc' = List.collect (fun c' -> splitCuboid c' (x,y,z)) acc
                    performInstructions acc' cs

// fsharp interactive timing and GC-output
#time
performInstructions [] data
|> printfn "Solution part 2: %d"    
