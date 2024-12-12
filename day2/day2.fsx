open System.IO
open System.Text.RegularExpressions

let inputFilepath = "day2_input.txt"
let input = File.ReadAllLines inputFilepath
let numberRegex = Regex(@"\s*(\d+)\s*")


type Monotonicity = Increasing | Decreasing | Constant

let isAtLeastOneAtMostThreeDiff number1 number2 = number1 - number2 >= 1 && number1 - number2 <= 3

let splitReportPairwise report = report |> Array.pairwise

let checkMonotonicityAndDifference report =
    report
    |> Array.map (fun (fst, snd) ->
        if fst > snd then (fst, snd, Decreasing, isAtLeastOneAtMostThreeDiff fst snd)
        elif snd > fst then (fst, snd, Increasing, isAtLeastOneAtMostThreeDiff snd fst)
        else (fst, snd, Constant, false)
        )

let safetyCheck annotatedReport =
    match (annotatedReport |> Array.distinctBy (fun (_,_,dir, pred) -> dir, pred)) with
    | [|_, _, Increasing, true|] | [|_, _, Decreasing, true|] -> true
    | _ -> false

let reports =
    input
    |> Array.map (fun report ->
        let matches = numberRegex.Matches report
        [|
         for level in matches do
             level.Value |> int
        |]
        )

let getSafeReportCount =
    reports
    |> Array.map (splitReportPairwise >> checkMonotonicityAndDifference >> safetyCheck)
    |> Array.where id
    |> Array.length

printfn "The number of safe reports is : %d" getSafeReportCount

// Just going to brute force enumeration
let dampenedSafetyCheck (report : int array) =
    [|0..(Array.length report - 1)|]
    |> Array.map (fun i ->
        report
        |> Array.removeAt i)
    |> Array.map (splitReportPairwise >> checkMonotonicityAndDifference >> safetyCheck)
    |> Array.exists id

let getSafeReportCountAfterDampening =
    reports
    |> Array.map dampenedSafetyCheck
    |> Array.where id
    |> Array.length

printfn "The number of safe reports after dampening is : %d" getSafeReportCountAfterDampening

let getMonotonicityGroups report =
    report
    |> splitReportPairwise
    |> Array.mapi (fun i (fst, snd) ->
        if fst > snd then (i, fst, snd, Decreasing, isAtLeastOneAtMostThreeDiff fst snd)
        elif snd > fst then (i, fst, snd, Increasing, isAtLeastOneAtMostThreeDiff snd fst)
        else (i, fst, snd, Constant, false)
        )
    |> Array.groupBy (fun (_, _, _, monotonicity, _) -> monotonicity)
    |> Array.sortBy (fun (_, dirGroup) -> Array.length dirGroup)

// Tried to use a decision tree but somewhere the reasoning is incorrect and doesn't produce the same answer as brute force, which gives the correct result
let safetyCheckUsingGroups (annotatedReportsGrouped: (Monotonicity * (int * int * int * Monotonicity * bool) array) array)=
    if Array.length annotatedReportsGrouped > 2 then false
    elif Array.length annotatedReportsGrouped = 2 then
        let deviantGroup = annotatedReportsGrouped |> Array.head
        let lengthOfDeviantGroup = deviantGroup |> snd |> Array.length
        if lengthOfDeviantGroup = 1 then
            let deviantGroupDirection = deviantGroup |> fst
            let deviantPair = deviantGroup |> snd
            let reportForDampening =
                annotatedReportsGrouped
                |> Array.last
                |> snd
                |> Array.append deviantPair
                |> Array.sortBy(fun (index, _, _, _, _) -> index)
            reportForDampening
            |> Array.map(fun (_,fst,snd,dir,_) -> fst,snd,dir)
            |> Array.fold (fun (acc, prevNumbers) (fst, snd, monotonicity) ->
                    if monotonicity = deviantGroupDirection then
                        match prevNumbers with
                        | [||] -> (acc, [||])
                        | _ ->
                            let modifiedPrevNumbers = 
                                if prevNumbers.Length > 1 then 
                                    prevNumbers[0..prevNumbers.Length-2] 
                                else 
                                    [||]
                            acc, modifiedPrevNumbers
                    else
                        let newAcc = Array.append acc [|(fst, snd, monotonicity)|]
                        let newPrevNumbers = Array.append prevNumbers [|snd|]
                        newAcc, newPrevNumbers
                ) ([||], [||])
            |> fst
            |> Array.map (fun (fst,snd,_) -> fst,snd)
            |> checkMonotonicityAndDifference 
            // |> safetyCheck
            |> Array.forall (fun (_, _, _, pred) -> pred)
        else false
    elif (Array.length annotatedReportsGrouped = 1) && (annotatedReportsGrouped |> Array.exactlyOne |> fun (fst, _) -> fst <> Constant) then
        annotatedReportsGrouped
        |> Array.exactlyOne
        |> snd
        |> Array.map(fun (_,fst,snd,dir,pred) -> fst,snd,dir,pred)
        |> safetyCheck
    else false

let dampenedSafeReportCount =
    reports
    |> Array.map (getMonotonicityGroups >> safetyCheckUsingGroups)
    |> Array.where id
    |> Array.length

printfn "The number of safe reports after dampening not is : %d" dampenedSafeReportCount

