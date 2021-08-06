﻿namespace BitcoinFromScratch

module Math = 
    // Takes in an uppercase hex sting with 0x prefix and converts it to a bigint base 10
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

    // Returns (gcd, x, y) s.t. a * x + b * y == gcd
    // This function implements the extended Euclidean algorithm.
    // (Figure out non mutaable version later)
    let ExtendedEuclideanAlgorithm a b = 
        let mutable (mutable_old_r, mutable_r, mutable_old_s, mutable_s, mutable_old_t, mutable_t) = (a, b, 1, 0, 0, 1)
        let CalculateStep (old_r, r, old_s, s, old_t, t) = 
            let quotient = int old_r / r
            (r, old_r - quotient * r, s, old_s - quotient * s, t, old_t - quotient * t)
        while mutable_r.Equals(0) = false do
            let (old_r, r, old_s, s, old_t, t) = CalculateStep (mutable_old_r, mutable_r, mutable_old_s, mutable_s, mutable_old_t, mutable_t)
            mutable_old_r <- old_r
            mutable_r <- r
            mutable_old_s <- old_s
            mutable_s <- s
            mutable_old_t <- old_t
            mutable_t <- t
        (mutable_old_r, mutable_old_s, mutable_old_t)

    // Returns modular multiplicate inverse m s.t. (n * m) % p == 1
    let inv n p =
        let (gcd, x, y) = ExtendedEuclideanAlgorithm n p
        x % p

module EllipticCurve = 
    // Elliptic curve over the finite filed of integers modulo a prime.
    // Points on the curve satisfy y^2 = x^3 + a*x + b
    type Curve = 
        val p: bigint // The prime modulus of the finite field
        val a: bigint
        val b: bigint
        new(p0, a0, b0) = { p = p0; a = a0; b = b0 }

    // An integer point (x,y) on a Curve
    type Point = 
        val curve: Curve
        val x: bigint
        val y: bigint
        val isINF: bool
        new(c, x0, y0) = { curve = c; x = x0; y = y0; isINF = false }
        new(inf) = { curve = new Curve(bigint 0, bigint 0, bigint 0); x = bigint 0; y = bigint 0; isINF = inf}

    // The generator over the Curve
    type Generator = 
        val G: Point // The generator
        val n: bigint // The order of the generator (implement Schoof's algo later)
        new(G0, n0) = { G = G0; n = n0 }

    // Point at infinity
    let INF = new Point(true)

    // True if point is on the curve
    let IsOnCurve (point: Point) = 
        if point.isINF = false then ((point.y)**2 - (point.x)**3 - point.curve.a * point.x - point.curve.b) % point.curve.p = (bigint 0) else true

    // y^2 = x^3 + 7
    let BitcoinCurve = new Curve(Math.HexStringToInt "0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFC2F", Math.HexStringToInt "0x0000000000000000000000000000000000000000000000000000000000000000", Math.HexStringToInt "0x0000000000000000000000000000000000000000000000000000000000000007")
        
    // Generator for the BitcoinCurve
    let BitcoinGenerator = new Generator(new Point(BitcoinCurve, Math.HexStringToInt "0x79BE667EF9DCBBAC55A06295CE870B07029BFCDB2DCE28D959F2815B16F81798", Math.HexStringToInt "0x483ADA7726A3C4655DA4FBFC0E1108A8FD17B448A68554199C47D08FFB10D4B8"), Math.HexStringToInt "0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEBAAEDCE6AF48A03BBFD25E8CD0364141")
    

module Main = 
    printfn $"done"

module test = 
    printfn "done"