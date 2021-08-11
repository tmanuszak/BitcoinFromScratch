// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp
open BitcoinFromScratch.EllipticCurve
open BitcoinFromScratch.Math

[<EntryPoint>]
let Main argv = 
    let pk = 5I * BitcoinGenerator.G
    let tf = IsOnCurve pk
    0