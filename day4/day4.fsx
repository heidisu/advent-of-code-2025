open System.IO

let grid =
    File.ReadLines "input.txt" |> Seq.toList |> List.map (fun l -> Seq.toList l)

let width = grid |> List.head |> List.length
let height = grid |> List.length


let neighbours =
    [ (1, 0); (-1, 0); (0, 1); (0, -1); (1, 1); (1, -1); (-1, 1); (-1, -1) ]

let neighboursCount y x =
    neighbours
    |> List.map (fun (a, b) -> (y + a, x + b))
    |> List.filter (fun (a, b) -> a >= 0 && a < height && b >= 0 && b < width)
    |> List.map (fun (a, b) -> grid[a][b])
    |> List.filter (fun c -> c = '@')
    |> List.length

let isForkliftAccessible y x =
    let paperrollneighbours =
        neighbours
        |> List.map (fun (a, b) -> (y + a, x + b))
        |> List.filter (fun (a, b) -> a >= 0 && a < height && b >= 0 && b < width)
        |> List.map (fun (a, b) -> grid[a][b])
        |> List.filter (fun c -> c = '@')

    List.length paperrollneighbours < 4

let rec removeRolls (paperRolls: Map<int * int, int>) acc =
    let accessible = paperRolls |> Map.filter (fun (y, x) c -> c < 4)

    if Map.count accessible = 0 then
        acc
    else
        let neigboursOfAccessible =
            accessible
            |> Map.keys
            |> Seq.collect (fun (y, x) -> neighbours |> List.map (fun (a, b) -> (y + a, x + b)))
            |> Seq.groupBy id
            |> Seq.map (fun ((y, x), l) -> (y, x), Seq.length l)

        let accessibleRemoved =
            accessible
            |> Map.keys
            |> Seq.fold (fun m p -> if Map.containsKey p m then Map.remove p m else m) paperRolls

        let updatedNeighbourCounts =
            neigboursOfAccessible
            |> Seq.fold
                (fun m (p, c) ->
                    if Map.containsKey p m then
                        Map.add p (Map.find p m - c) m
                    else
                        m)
                accessibleRemoved

        removeRolls updatedNeighbourCounts (acc + Map.count accessible)


let task1 =
    [ 0 .. height - 1 ]
    |> List.collect (fun y ->
        [ 0 .. width - 1 ]
        |> List.map (fun x -> grid[y][x] = '@' && isForkliftAccessible y x))
    |> List.filter id
    |> List.length

let task2 =
    let paperRolls =
        [ 0 .. height - 1 ]
        |> List.collect (fun y -> [ 0 .. width - 1 ] |> List.map (fun x -> (y, x)))
        |> List.filter (fun (y, x) -> grid[y][x] = '@')
        |> List.map (fun (y, x) -> (y, x), neighboursCount y x)
        |> Map.ofList

    removeRolls paperRolls 0


printfn $"Task 1: {task1}"
printfn $"Task 2: {task2}"
