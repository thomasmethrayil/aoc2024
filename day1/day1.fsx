open System.IO
open System.Text.RegularExpressions

let inputFilepath = "day1_input.txt"
let fileLines = File.ReadAllLines inputFilepath
let numberRegex = Regex(@"\s*(\d+)\s*")

let getDiff value1 value2 =
    if value1 > value2 then value1 - value2
    elif value2 > value1 then value2 - value1
    else 0

let list1, list2 =
    fileLines
    |> Array.map (fun line ->
        let matchResult = numberRegex.Matches(line)
        matchResult.Item(0).Value |> int, matchResult.Item(1).Value |> int)
    |> Array.unzip
    |> fun (l1, l2) -> Array.sort l1, Array.sort l2

let distances = Array.map2 getDiff list1 list2

let task1Result = distances |> Array.sum
printfn "Total distance between lists: %d" task1Result

let occurenceMap =
    list2
    |> Array.groupBy id
    |> Array.map (fun (integer, group) -> integer, Array.length group)
    |> Map.ofArray

let getFrequency number = Map.tryFind number occurenceMap

let similiarityScore =
    list1
    |> Array.map (fun num ->
        let numFrequency =
            match getFrequency num with
            | Some number -> number
            | None -> 0

        num * numFrequency)
    |> Array.sum

printfn "The similarity score is : %d" similiarityScore
