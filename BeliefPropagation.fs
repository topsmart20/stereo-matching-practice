// Currently just trying to focus on a basic version of min-sum 2D loopy belief propagation
// Using min-sum on the same basis that Felzenswalb & Huttenlocher substituted max-product with it

module BeliefPropagation

open Common
open System

//[<Struct; IsByRefLike>]
type BPParameters<'a,'b> = {
    dataFunction: 'a -> 'a -> 'b
    //dataCosts: 'b [] []
    smoothnessFunction: int -> int -> 'b
    //smoothnessCosts: float32[,]
    iterations: int
}

let computeNeighbours parameters i =
    let x = i % parameters.width
    let y = i / parameters.width

    [|
        if y > 0 then yield i - parameters.width;
        if x > 0 then yield i - 1;
        if x < (parameters.width - 1) then yield i + 1;
        if y < (parameters.height - 1) then yield i + parameters.width;
    |]

let inline initMessages parameters =
    Array.init (parameters.width * parameters.height) (
        fun i ->
                let neighbours = computeNeighbours parameters i
                Array.map (fun neighbour -> (neighbour, Array.zeroCreate parameters.maximumDisparity)) neighbours
    )

let updateMessages maxD (dataCosts : float32 [][]) (smoothnessCosts : float32 [,])
    (m1 : (int * float32 [])[][]) (m2 : (int * float32 [])[][]) =
// this function computes updates to the messages using data in m1, and stores it back to m2
// p and q are used below in accordance with Felzenswalb & Huttenlocher's notation
    m1 |> Array.iteri(fun i p -> // each pixel in the image
        let neighbourMessageSums = Array.zeroCreate maxD
        for fp = 0 to maxD do
            for (_, neighbourCosts) in p do
                neighbourMessageSums.[fp] <- neighbourMessageSums.[fp] + neighbourCosts.[fp] + dataCosts.[i].[fp]
                // Strictly speaking, data costs shouldn't be here, but since it is all additions, and data costs vary only by fp
                // It's easier just to include them here
        for (neighbourIdx, neighbourCosts) in p do // each neighbour of the current pixel
            let indexInNeighbour = Array.findIndex (fst >> ((=) i)) m1.[neighbourIdx]
            for fq = 0 to maxD do // each disparity label of q
                let mutable mincost = LanguagePrimitives.GenericZero
                for fp = 0 to maxD do
                    let smoothnessCost = smoothnessCosts.[fp, fq]
                    let previousMessageCost = neighbourMessageSums.[fp] - neighbourCosts.[fp]
                    let totalCost = smoothnessCost + previousMessageCost
                    if totalCost < mincost then
                        mincost <- totalCost

            m2.[neighbourIdx].[indexInNeighbour].[fq] <- mincost
    )

let computeFinalDisparities parameters messages =
    Array.zeroCreate parameters.totalPixels



let beliefpropagation parameters bpparameters =
    //let neighbours = Array.init (parameters.width * parameters.height) (computeNeighbours parameters)
    //let dataCosts = Data.computeDataCosts parameters Data.absoluteDifference // need to pass in the images, at least
    let dataCosts = Data.computeDataCosts parameters bpparameters.dataFunction float32
    //let smoothnessCosts = Smoothness.computeSmoothnessCosts parameters (Smoothness.pottsFloat32 Smoothness.LAMBDA_FH) // will need the maximum disparity for the smoothness costs
    let smoothnessCosts = Smoothness.computeSmoothnessCosts parameters bpparameters.smoothnessFunction float32
    let mutable messages1 = initMessages parameters // All messages will be 0 initially
    let mutable messages2 = Array.copy messages1
    for i = 1 to bpparameters.iterations do
        updateMessages parameters.maximumDisparity dataCosts smoothnessCosts messages1 messages2
        let temp = messages1
        messages1 <- messages2
        messages2 <- temp

    computeFinalDisparities parameters messages1