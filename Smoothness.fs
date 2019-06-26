module Smoothness

[<Literal>]
let LAMBDA_FH = 0.07f
[<Literal>]
let TAU_FH = 15.0f
[<Literal>]
let D_FH = 1.7f

// let inline potts lambda a b =
//     if a = b then
//         LanguagePrimitives.GenericZero
//     else
//         lambda

let inline potts lambda a b =
    if a = b then
        0.0f
    else
        lambda

let inline pottsFloat32 (lambda: float32) (a : float32) b =
    if System.Math.Abs(a - b) < System.Single.Epsilon then
        0.0f
    else
        lambda

// d here matches to 'd' used in Felzenswalb & Huttenlocher (2006) - it is a truncation parameter
let inline truncatedLinear lambda d a b =
    (min (Data.absoluteDifference a b) d) |> float |> (*) lambda

let inline truncatedQuadratic lambda d a b =
    (min (Data.squaredDifference a b) d) |> float |> (*) lambda

let inline computeSmoothnessCosts (parameters : Common.Parameters) (smoothnessFunction : int -> int -> single) =
    Array2D.init parameters.maximumDisparity parameters.maximumDisparity smoothnessFunction