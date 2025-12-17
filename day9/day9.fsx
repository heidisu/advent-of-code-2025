#time on

open System.IO
open System.Collections.Generic

let readFile () =
    File.ReadLines "input.txt"
    |> Seq.toList
    |> List.map (fun s ->
        let parts = s.Split(",")
        (int64 parts[0], int64 parts[1]))

let points = readFile ()
let size = List.length points

let pairs =
    [ 0 .. size - 1 ]
    |> List.collect (fun x -> [ x + 1 .. size - 1 ] |> List.map (fun y -> (points[x], points[y])))

let area (a, b) (c, d) = (abs (a - c) + 1L) * (abs (b - d) + 1L)

let isInside sortedXvalues (a, b) =
    let rightIntersections = sortedXvalues |> List.skipWhile (fun x -> x <= a)

    let isTangent =
        rightIntersections |> List.windowed 2 |> List.exists (fun l -> l[0] + 1L = l[1])

    if not isTangent then
        List.length rightIntersections % 2 = 1
    else
        let leftIntersections = sortedXvalues |> List.takeWhile (fun x -> x < a)

        let isTangent =
            leftIntersections |> List.windowed 2 |> List.exists (fun l -> l[0] + 1L = l[1])

        if isTangent then
            failwith "both tangents"

        List.length leftIntersections % 2 = 1

let boundary =
    points @ [ List.head points ]
    |> List.windowed 2
    |> List.map (fun l -> (l[0], l[1]))

let allRedTiles =
    boundary
    |> List.collect (fun ((a, b), (c, d)) ->
        let xMin = min a c
        let xMax = max a c
        let yMin = min b d
        let yMax = max b d

        if a = c then
            [ yMin..yMax ] |> List.map (fun y -> (a, y))
        else
            [ xMin..xMax ] |> List.map (fun x -> (x, b)))

let allRedTileSet = Set.ofList allRedTiles

let redTilesMap =
    allRedTiles
    |> List.map snd
    |> List.distinct
    |> List.map (fun y ->
        (y,
         allRedTiles
         |> List.filter (fun (a, b) -> b = y)
         |> List.map fst
         |> List.distinct
         |> List.sort))
    |> Map.ofList

let validIntervals = Dictionary<(int64 * int64) * (int64 * int64), bool>()

let isValidInterval xVals xMin xMax x y =
    match xMin, xMax with
    | Some xMin, Some xMax ->
        match validIntervals.TryGetValue(((xMin, y), (xMax, y))) with
        | true, isValid -> isValid
        | _ ->
            let isValid = isInside xVals (x, y)
            validIntervals.Add(((xMin, y), (xMax, y)), isValid)
            isValid
    | _, _ -> false

let hasValidPointInSameBoundary a1 a2 b =
    let xVals = Map.find b redTilesMap
    let xValsRev = List.rev xVals
    let a1Min = xValsRev |> List.tryFind (fun x -> x < a1)
    let a1Max = xVals |> List.tryFind (fun x -> x > a1)
    let a2Min = xValsRev |> List.tryFind (fun x -> x < a2)
    let a2Max = xVals |> List.tryFind (fun x -> x > a2)

    if a1Min = a2Min && a1Max = a2Max then
        isValidInterval xVals a1Max a1Max (a1 + 1L) b
    else
        false

let isValidPoint (a, b) =
    if Set.contains (a, b) allRedTileSet then
        true
    else
        match Map.tryFind b redTilesMap with
        | Some xVals ->
            let xMin = xVals |> List.rev |> List.tryFind (fun x -> x < a)
            let xMax = xVals |> List.tryFind (fun x -> x > a)
            isValidInterval xVals xMin xMax a b
        | None -> false


let task1 = pairs |> List.map (fun (x, y) -> area x y) |> List.max

let task2 =
    pairs
    |> List.map (fun ((x, y)) -> ((x, y), area x y))
    |> List.sortByDescending (fun (_, a) -> a)
    |> List.find (fun (((a, b), (c, d)), ar) ->

        let xMin = min a c
        let xMax = max a c
        let yMin = min b d
        let yMax = max b d
        let hasInnerPoins = xMax - xMin > 2L
        let corners = [ (xMin, yMin); (xMin, yMax); (xMax, yMin); (xMax, yMax) ]

        if corners |> List.forall isValidPoint then
            [ yMin..yMax ]
            |> List.forall (fun y ->
                (hasInnerPoins && hasValidPointInSameBoundary (xMin + 1L) (xMax - 1L) y)
                || [ xMin..xMax ] |> List.forall (fun x -> isValidPoint (x, y)))
        else
            false)
    |> snd

printfn $"Task 1: {task1}" // 4715966250
printfn $"Task 2: {task2}" //1530527040
