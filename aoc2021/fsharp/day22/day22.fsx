printfn "Timing function definitions, reading input data and parsing it.."
let defTime = System.Diagnostics.Stopwatch.StartNew()
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

let span (start:int64) stop =
    abs (max start stop - min start stop) + 1L
let countCuboid ((x0,x1),(y0,y1),(z0,z1)) = span x0 x1 * span y0 y1 * span z0 z1
let countCuboids cs = List.fold (fun acc x -> countCuboid x + acc) 0L cs

// So, we are dealing with cuboids of dims X*Y*Z
// We need to run the reboot procedure for all cubes in an infinite 3d-grid
// We clearly can't make an array of size a billion
// (Even if we can, it's a lot faster and more fun not to waste that much space)
// Algorithm:
// When we
// Turn off a range:
//     We split current cuboids, st.
//     for each current cuboid, we take the intersection with the "turn off cuboid" and remove those.
// Turn on a range:
//     We do the same as when turning off, i.e. split all cuboids by the new cuboid
//     leaving only cuboids with no intersection with the new cuboid.
//     Now we can simply add the new cuboid. 

// Find the intersection of c1 and c2
// Create n {0..6} new cuboids, from all the points in c1 that do not intersect with c2
let splitCuboid c1 c2 =
    let (x0,x1),(y0,y1),(z0,z1) = c1
    let (x0',x1'),(y0',y1'),(z0',z1') = c2
    // Do we have no overlap? (separate statements are ugly, but for some reason slightly faster than 6 OR operations)
    // is c2 to the left or right of c1?
    if x1' < x0 || x1 < x0' then [c1]
    // is c2 above or below c1? 
    else if y1' < y0 || y1 < y0' then [c1]
    // Is c2 in front of or behind c1?
    else if z1' < z0 || z1 < z0' then [c1]
    // Is c1 fully contained in c2? Then we just remove c1
    else if x0' <= x0 && x1 <= x1' && y0' <= y0 && y1 <= y1' && z0' <= z0 && z1 <= z1' then []
    // Well, we have an overlap, so split c1 into smaller cubes
    else
        // We need l,r,u,d,f,b (6)
        // as well as lf,lb,rf,rb (4)
        // and luc,luf,lub,ldc,ldf,ldb (6)
        // and ruc,ruf,rub,rdc,rdf,rdb (6)
        // For a total maximum of 26 cubes (dear lord)
        // Instead we do something a bit more clever, and take:
        // Full left side (l,lf,lb, luc,luf,lub,ldc,ldf,ldb) -> 9 pieces in one
        // Full right side (r,rf,rb, ruc,ruf,rub,rdc,rdf,rdb) -> 9 pieces in one
        // full bottom minus l/r-sides (d,df,db) - 3 pieces
        // Full top minus l/r-sides (u,uf,ub) -> 3 pieces
        // f by itself -> 1 piece
        // b by itself -> 1 piece
        let cx = max x0 x0',min x1 x1'
        let cy = max y0 y0',min y1 y1'
        let nc1 =
            seq {
                    // Full left
                    if x0 < x0' then yield (x0,x0'-1L),(y0,y1),(z0,z1)
                    // Full right
                    if x1 > x1' then yield (x1'+1L,x1),(y0,y1),(z0,z1)
                    // bottom without l/r
                    if y0 < y0' then yield cx,(y0,y0'-1L),(z0,z1)                    
                    // Top without l/r
                    if y1' < y1 then yield cx,(y1'+1L,y1),(z0,z1)
                    // Forward
                    if z0 < z0' then yield cx,cy,(z0,z0'-1L)
                    // Backward
                    if z1' < z1 then yield cx,cy,(z1'+1L,z1)
                }
        nc1 |> Seq.toList

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

let data = readFile "input.txt"
// For part 1, only consider region (-50..50) for each coord
let part1Data =
    let inRange (start,stop) = -50L <= start && start <= 50L && -50L <= stop && stop <= 50L
    let part1Filter = function
        | On (x,y,z) -> inRange x && inRange y && inRange z
        | Off (x,y,z) -> inRange x && inRange y && inRange z    
    List.filter part1Filter data

defTime.Stop()
printfn "%O" defTime.Elapsed

printfn "Timing computation of part 1"
let p1time = System.Diagnostics.Stopwatch.StartNew()
let part1res = performInstructions [] part1Data
p1time.Stop()
printfn "%O" p1time.Elapsed
printfn "Solution part 1: %d" part1res

printfn "Timing computation of part 2"
let p2time = System.Diagnostics.Stopwatch.StartNew()
let part2res = performInstructions [] data
p2time.Stop()
printfn "%O" p2time.Elapsed
printfn "Solution part 2: %d" part2res
