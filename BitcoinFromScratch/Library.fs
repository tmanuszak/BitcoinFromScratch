namespace BitcoinFromScratch

module EllipticCurve = 
    type Curve = 
        { 
            p: bigint
            a: bigint
            b: bigint 
        }

    type Point = 
        val curve: Curve
        val x: bigint
        val y: bigint
        new(c, x0, y0) = { curve = c; x = x0; y = y0 }

    let HexStringToInt (str: string) =
        let rec Number (str: string) (index: int) (length: int) (b: bigint, d: bigint) = 
            let result = 
                if index = length - 1 then  // Reached end of string, now start calculating
                    if str.[index] >= '0' && str.[index] <= '9' then b * (bigint 16), d + (bigint ((int str.[index]) - 48)) * b  // Character is 0-9
                    elif str.[index] >= 'A' && str.[index] <= 'F' then b * (bigint 16), d + (bigint ((int str.[index]) - 55)) * b  // Character is A-F
                    else  // There is a bug if you are in here
                        printfn $"Here! {index} {length} {b} {d}"
                        bigint -1, bigint -1
                else 
                    let (bresult, dresult) = Number str (index + 1) length (b, d)  // Not reached end of string. Keep recursing down then calculate on the way up.
                    if str.[index] >= '0' && str.[index] <= '9' then bresult * (bigint 16), dresult + (bigint ((int str.[index]) - 48)) * bresult  // Character is 0-9
                    elif str.[index] >= 'A' && str.[index] <= 'F' then bresult * (bigint 16), dresult + (bigint ((int str.[index]) - 55)) * bresult  // Character is A-F
                    else  // There is a bug if you are in here
                        printfn $"Here!! {index} {length} {b} {d}"
                        bigint -1, bigint -1
            result
        snd <| Number str 2 str.Length (bigint 1, bigint 0)

    let BitcoinCurve = 
        { 
            p = HexStringToInt "0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFC2F"
            a = HexStringToInt "0x0000000000000000000000000000000000000000000000000000000000000000"
            b = HexStringToInt "0x0000000000000000000000000000000000000000000000000000000000000007"
        }

    let G = new Point(BitcoinCurve, HexStringToInt "0x79BE667EF9DCBBAC55A06295CE870B07029BFCDB2DCE28D959F2815B16F81798", HexStringToInt "0x483ADA7726A3C4655DA4FBFC0E1108A8FD17B448A68554199C47D08FFB10D4B8")
    let OnCurve = ((G.y)**2 - (G.x)**3 - (bigint 7)) % BitcoinCurve.p = (bigint 0)
    printfn $"Is on curve {OnCurve}"
        
        

module Main = 
    printfn $"done"

module test = 
    printfn "done"