open System.IO
open System.Text.RegularExpressions


type CPU = { A: int; B: int; C: int; PC: int }



let (|Integer|_|) (str: string) =
   let mutable intvalue = 0
   if System.Int32.TryParse(str, &intvalue) then Some(intvalue)
   else None

let getInt = function
    | Integer i -> i
    | x -> failwith <| sprintf "Bad data: %A" x

let (|ParseRegex|_|) regex str =
   let m = Regex(regex).Match(str)
   if m.Success
   then Some (List.tail [ for x in m.Groups -> x.Value ])
   else None

let (|RegisterA|_|) = function
    | ParseRegex "Register A: (\d+)" [Integer a] -> Some a
    | _ -> None

let (|RegisterB|_|) = function
    | ParseRegex "Register B: (\d+)" [Integer b] -> Some b
    | _ -> None

let (|RegisterC|_|) = function
    | ParseRegex "Register C: (\d+)" [Integer c] -> Some c
    | _ -> None

let getRegister = function
    | RegisterA a -> a
    | RegisterB b -> b
    | RegisterC c -> c
    | x -> failwith <| sprintf "Bad data: %A" x


let parseCPU (s:string) =
    s.Split "\n"
    |> Array.filter ((<>) "")
    |> Array.map getRegister
    |> (fun arr -> { A = arr.[0]; B = arr.[1]; C = arr.[2] ; PC = 0 })


let parseProgram (s:string) =
    s.Split ": "
    |> fun arr -> arr.[1]
    |> fun s -> s.Split ","
    |> Array.filter ((<>) "")
    |> Array.mapi (fun i x -> i,getInt x)
    |> Map.ofArray

let readFile file =
    File.ReadAllText file
    |> fun s -> s.Split "\n\n"
    |> (fun a -> parseCPU a.[0],parseProgram a.[1])
    

//let data = readFile "test.txt"
let data = readFile "input.txt"

let getComboOperand cpu operand =
    match operand with
        | 4 -> cpu.A
        | 5 -> cpu.B
        | 6 -> cpu.C
        | 7 -> failwith <| sprintf "Invalid combo operand: %A" operand
        | _ -> operand

let nextInstruction program cpu =
//    printfn "cpu: %A" cpu
    let opcode = Map.tryFind cpu.PC program
    if Option.isNone opcode then
        printfn "HALTING"
        None
    else
    let operand =
        match Map.tryFind (cpu.PC+1) program with
            | None -> failwith <| sprintf "Found no operand at %A" (cpu.PC+1)
            | Some op -> op

    //printfn "Found opcode: %A and operand: A" opcode operand
    match opcode with
        None ->
            printfn "HALTING"
            None
        // adv - division
        | Some 0 ->
            let operand' = getComboOperand cpu operand
            let shift = pown 2 operand'            
//            let shift =
//                if (operand' - 1 = 0) then 1 else 2 <<< (operand' - 1)
                //2 <<< (operand' - 1)

            let res = cpu.A / shift
//            printfn "operand: %A, res: %A" operand' res
            Some { cpu with A = res; PC = cpu.PC+2 }
        // bxl - bitwise XOR of B and operand
        | Some 1 ->
            let res = cpu.B ^^^ operand
            Some { cpu with B = res; PC = cpu.PC+2 }
        // bst
        | Some 2 ->
            let combo = getComboOperand cpu operand
            let res = combo % 8
            Some { cpu with B = res; PC = cpu.PC + 2 }
        // jnz
        | Some 3 -> 
            if cpu.A = 0 then
                // printfn "Not jumping"
                Some { cpu with PC = cpu.PC + 2 }
            else
                // printfn "Jumping to: %A" operand
                Some { cpu with PC = operand }
        
        // bxc
        | Some 4 ->
            let res = cpu.B ^^^ cpu.C
            Some { cpu with B = res; PC = cpu.PC + 2 }
        
        // out
        | Some 5 ->
            let combo = (getComboOperand cpu operand) % 8
            printf "%d," combo
            Some { cpu with PC = cpu.PC + 2 }
        
        // bdv
        | Some 6 ->
            let operand' = getComboOperand cpu operand
            let shift = pown 2 operand'
            //    if (operand' - 1 = 0) then 1 else 2 <<< (operand' - 1)                
                //2 <<< (operand' - 1)
            let res = cpu.A / shift
            Some { cpu with B = res; PC = cpu.PC+2 }
            
        // cdv
        | Some 7 ->
            let operand' = getComboOperand cpu operand
            let shift = pown 2 operand'
                // if (operand' - 1 = 0) then 1 else 2 <<< (operand' - 1)
            // printfn "cpu: %A" cpu
            // printfn "operand: %A, operand': %A, shift: %A" operand operand' shift
            let res = cpu.A / shift
            Some { cpu with C = res; PC = cpu.PC+2 }


        | unexpected -> failwith <| sprintf "Unexpected opcode: %A" unexpected
        


let rec run (cpu,program) =
    match nextInstruction program cpu with
        | None -> ()
        | Some cpu' -> run (cpu',program)

run data

