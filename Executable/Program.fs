// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp
open BitcoinFromScratch.EllipticCurve
open BitcoinFromScratch.Math

[<EntryPoint>]
let Main argv = 
    let pk = 22265090479312778178772228083027296664144I * BitcoinGenerator.G
    let tf = IsOnCurve pk
    printfn $"{IsOnCurve pk}\n{pk.x}\n{pk.y}"
    0