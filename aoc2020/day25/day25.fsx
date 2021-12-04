
let getLoopSize s publicKey =
    let mutable value = 1L
    let mutable loopSize = 0L
    while value <> publicKey do
        value <- value * s
        value <- value % 20201227L
        loopSize <- loopSize + 1L
    loopSize

let getEncryptionKey loopSize publicKey =
    let mutable encryptionKey = 1L
    for i = 1 to loopSize do
        encryptionKey <- encryptionKey * publicKey
        encryptionKey <- encryptionKey % 20201227L
    encryptionKey

// Testing
// let cpk = 5764801L
// let dpk = 17807724L
// let cls = getLoopSize 7L cpk |> int
// let dls = getLoopSize 7L dpk |> int

// let ek1 = getEncryptionKey cls dpk
// let ek2 = getEncryptionKey dls cpk

// Input data
let cpk = 9789649L
let dpk = 3647239L
let cls = getLoopSize 7L cpk |> int
let dls = getLoopSize 7L dpk |> int

let ek1 = getEncryptionKey cls dpk
let ek2 = getEncryptionKey dls cpk
if ek1 = ek2 then printfn "Part 1 solution: %d" ek1
else printfn "Oh dang, computed encryption key wrong"
