// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp
open BitcoinFromScratch.EllipticCurve

[<EntryPoint>]
let Main argv = 
    let sk = 3
    let pk = BitcoinGenerator.G + BitcoinGenerator.G + BitcoinGenerator.G
    printfn $"Secret Key: {sk}\nPublic Key: {pk.x}, {pk.y}"
    printfn $"Is on curve: {IsOnCurve pk}"
    0