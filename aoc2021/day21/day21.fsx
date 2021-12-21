// Infinite cycle of die-rolls with deterministic 100-sided die
let rec cycle xs = seq { yield! xs; yield! cycle xs }
let die = seq { 1 .. 100 } |> cycle
let roll (die: seq<int>) = Seq.take 3 die |> Seq.sum, Seq.skip 3 die

let newPos pos dieRoll = ((pos + dieRoll - 1) % 10) + 1

let rec game p1 p2 die dierolls =
    let p1p,p1s = p1
    let p2p,p2s = p2
    // p2 lost
    if p1s >= 1000 then p2s * (dierolls)
    // p1 lost
    else if p2s >= 1000 then p1s * (dierolls)
    // Game goes on!
    else
        let rollsum,newdie = roll die
        let p1p' = newPos p1p  rollsum
        let p1s' = p1s + p1p'
        game p2 (p1p',p1s') newdie (dierolls+3)

// test
let p1t = 4,0
let p2t = 8,0

// Input
let p1 = 8,0
let p2 = 7,0

// Part 1
game p1 p2 die 0
|> printfn "Solution part 1: %A"
// Part 2
// Quantum dice, each roll yields 1,2 and 3
// We need 3 rolls, for a total of 27 universes
// But we just need the sum, totalling 7 universes, each  with count c 
let diep2 =
    seq {
          for r1 in 1 .. 3 do
          for r2 in 1 .. 3 do
          for r3 in 1 .. 3 do
          yield r1 + r2 + r3 } |> Seq.countBy id |> Seq.map (fun (a,b) -> a,uint64 b) |> Seq.cache

// add tuples
let addTups (a1:uint64,b1:uint64) (a2,b2) = a1+a2,b1+b2

// Play a quantum game
let rec game' p1 p2 state c p1turn =
    let p1p,p1s = p1
    let p2p,p2s = p2
    // If a player won, they one c times
    if p1s >= 21 then addTups state (c,0UL)
    else if p2s >= 21 then addTups state (0UL,c)
    // Game goes on! 
    else if p1turn then
        // Play a game in each universe with p1 to go
        // For each c' possible outcome of 3 dicerolls
        Seq.fold (fun acc (x,c') ->
                  // we have c' new positions/scores
                  let gp1 = let np = newPos p1p x in np,p1s+np
                  // This outcome will happen c * c' times
                  game' gp1 p2 acc (c*c') false) state diep2
    else
        // Play a game in each universe with p2 to go
        // For each c' possible outcome of 3 dicerolls
        Seq.fold (fun acc (x,c') ->
                  // we have c' new positions/scores                  
                  let gp2 = let np = newPos p2p x in np,p2s+np
                  // This outcome will happen c * c' times
                  game' p1 gp2 acc (c*c') true) state diep2



game' p1 p2 (0UL,0UL) 1UL true ||> max
|> printfn "Solution part 2: %A"

