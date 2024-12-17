open System.IO
open System.Text.RegularExpressions

type CPU = { A: int64; B: int64; C: int64; PC: int ; Output: int list }

let (|Integer|_|) (str: string) =
   let mutable intvalue = 0L
   if System.Int64.TryParse(str, &intvalue) then Some(intvalue)
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
    |> (fun arr -> { A = arr.[0]; B = arr.[1]; C = arr.[2] ; PC = 0 ; Output = [] })

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

let getComboOperand cpu operand =
    match operand with
        | 4L -> cpu.A
        | 5L -> cpu.B
        | 6L -> cpu.C
        | 7L -> failwith <| sprintf "Invalid combo operand: %A" operand
        | _ -> operand

let nextInstruction program cpu =
    match Map.tryFind cpu.PC program, Map.tryFind (cpu.PC+1) program with
        | None,_ -> false,cpu
        | Some opcode, None -> failwith <| sprintf "Found no operand at %A" (cpu.PC + 1)
        | Some opcode, Some operand ->
            match opcode with
                // adv - division
                | 0L ->
                    let operand' = getComboOperand cpu operand
                    let shift = pown 2 (int operand')
                    let res = cpu.A / (int64 shift)
                    true, { cpu with A = res; PC = cpu.PC+2 }
                // bxl - bitwise XOR of B and operand
                | 1L ->
                    let res = cpu.B ^^^ operand
                    true, { cpu with B = res; PC = cpu.PC+2 }
                // bst
                | 2L ->
                    let combo = getComboOperand cpu operand
                    let res = combo % 8L
                    true, { cpu with B = res; PC = cpu.PC + 2 }
                // jnz
                | 3L ->
                    if cpu.A = 0L then
                        true, { cpu with PC = cpu.PC + 2 }
                    else
                        true, { cpu with PC = int operand }

                // bxc
                | 4L ->
                    let res = cpu.B ^^^ cpu.C
                    true, { cpu with B = res; PC = cpu.PC + 2 }

                // out
                | 5L ->
                    let combo = (getComboOperand cpu operand) % 8L |> int
                    true, { cpu with PC = cpu.PC + 2 ; Output = combo :: cpu.Output }

                // bdv
                | 6L ->
                    let operand' = getComboOperand cpu operand
                    let shift = pown 2 (int operand')
                    let res = cpu.A / (int64 shift)
                    true, { cpu with B = res; PC = cpu.PC+2 }

                // cdv
                | 7L ->
                    let operand' = getComboOperand cpu operand
                    let shift = pown 2 (int operand')
                    let res = cpu.A / (int64 shift)
                    true, { cpu with C = res; PC = cpu.PC+2 }

                | unexpected -> failwith <| sprintf "Unexpected opcode: %A" unexpected

let rec run (cpu,program) =
    match nextInstruction program cpu with
        | false,cpu' -> cpu'
        | true, cpu' -> run (cpu',program)

//let data = readFile "test.txt"
//let data = readFile "test2.txt"

let data = readFile "input.txt"

run data
|> (fun c -> c.Output |> List.rev)
|> List.map string
|> List.reduceBack (fun x acc -> x + "," + acc)
|> printfn "Part 1: %s"


let program' = Map.toList (snd data) |> List.sort |> List.map snd |> List.map int

let basecpu = { A=0; B=0; C=0; PC=0; Output= [] }
let rec cursed p a depth =
    let mutable res = []
    if depth > List.length p then res
    else
        let tmp = a * 8L
        for i = 0 to 7 do
            let tmpRes = run ({basecpu with A=tmp+(int64 i) }, (snd data)) |> fun c -> c.Output |> List.rev
            let p' = List.rev p |> List.take (min (List.length p) (depth + 1)) |> List.rev
            if tmpRes = p' || List.skip 1 tmpRes = p' then
                if (depth + 1 = List.length p) then
                    res <- res @ [tmp+ (int64 i)]
                res <- res @ (cursed p (tmp + (int64 i)) (depth+1))
        res

let res = cursed program' 0L 0

res
|> List.min
|> printfn "Part 2: %A" 

