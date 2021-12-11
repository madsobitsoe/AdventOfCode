open System.IO
open System.Text.RegularExpressions


type Instruction =
    | Nop of int
    | Acc of int
    | Jmp of int
    

type Program = Map<int, Instruction>

// Part 1
// type State = { pc:int; log:Set<int>; acc:int } 
// let startState = {pc = 1; log = set []; acc = 0 } 
// Part 2
type State = { pc:int; log:Set<int>; acc:int; executed:int list } 


let startState = { pc = 1; log = set []; acc = 0; executed  = [] }

// Parsing
let explode (s:string) : char list = [for c in s do yield c]

// Partial active pattern for parsing an int
let (|Integer|_|) (str: string) =
   let mutable intvalue = 0
   if System.Int32.TryParse(str, &intvalue) then Some(intvalue)
   else None


let (|ParseRegex|_|) regex str =
   let m = Regex(regex).Match(str)
   if m.Success
   then Some (List.tail [ for x in m.Groups -> x.Value ])
   else None


let (|NopInst|_|) = function
    | ParseRegex "nop ([+-][0-9]+)" [Integer i] ->  Some (Nop i)
    | _ -> None
    
let (|AccInst|_|) = function
    | ParseRegex "acc ([+-][0-9]+)"  [Integer i] -> Some (Acc i)
    | _ -> None
    
let (|JmpInst|_|) = function
    | ParseRegex "jmp ([+-][0-9]+)" [Integer i] -> Some (Jmp i)
    | _ -> None
    
let parseInstruction = function
    | NopInst nop -> nop
    | AccInst acc -> acc
    | JmpInst jmp -> jmp
    | _ -> failwith "Unknown instruction"
    


let rec addLineNumbers map n = function
    | [] -> map
    | x::xs -> addLineNumbers (Map.add n x map) (n+1) xs
    

let readFile file  : Program =
    File.ReadAllLines file
    |> List.ofArray
    |> List.map parseInstruction
    |> addLineNumbers (Map []) 1


// Part 1 eval
let rec eval (state:State) program =
    if Set.contains state.pc state.log then state.acc
    else
        let next = Map.find state.pc program
        match next with
            | Nop n -> eval ({ state with pc = state.pc + 1; log = Set.add state.pc state.log }) program
            | Acc n -> eval ({ state with pc = state.pc + 1; log = Set.add state.pc state.log; acc = state.acc + n  }) program
            | Jmp n -> eval ({ state with pc = state.pc + n; log = Set.add state.pc state.log }) program
    
    
let test = readFile "test.txt"
let data = readFile "input.txt"


eval startState data
|> printfn "Solution part 1: %d"


// Part 2

// Recurse through the program from instruction n, replacing the first instruction found
let rec replaceInstruction (prog:Program) n =
    match prog.[n] with
        | Acc _ -> replaceInstruction prog (n+1)
        | Nop n' -> n,Map.add n (Jmp n') prog
        | Jmp n' -> n,Map.add n (Nop n') prog

let rec eval' origProg n prog =
    // Helper function for evaluating a program
    let rec helper state prog =
        // If we go +OOB, the program terminated and we are done
        if state.pc > Map.count prog then Some (state.acc)
        // If the current instruction is a repeat, return None to stop evaluation
        // and indicate a repeated instruction
        else if Set.contains state.pc state.log then
            None
        // In all other cases, evaluate next instruction and recurse
        else
            match Map.find state.pc prog with
            | Nop n -> helper ({ state with pc = state.pc + 1; log = Set.add state.pc state.log }) prog
            | Acc n -> helper ({ state with pc = state.pc + 1; log = Set.add state.pc state.log; acc = state.acc + n  }) prog
            | Jmp n -> helper ({ state with pc = state.pc + n; log = Set.add state.pc state.log }) prog

    // Evaluate the input prog
    match helper startState prog with
        // If none, we replace the "next possible instruction", initially starting at index 1
        // Then evaluate the resulting program recursively
        | None ->
            let n', newProg = replaceInstruction origProg n
            eval' origProg (n' + 1) newProg
        // If we terminated, we are done.
        | Some acc -> acc


eval' data 1 data
|> printfn "Solution part 2: %d"
