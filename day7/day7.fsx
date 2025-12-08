open System.IO

let readFile () =
    File.ReadLines "input.txt" |> Seq.toList |> List.map Seq.toList

let grid = readFile ()
let height = List.length grid
let startX = grid[0] |> List.findIndex (fun c -> c = 'S')

let rec findSplits splits positions y =
    if y >= height then
        splits
    else
        let (splits, newPos) =
            List.fold
                (fun (splits, newPos) x ->
                    match grid[y][x] with
                    | '^' -> (splits + 1, newPos @ [ x - 1; x + 1 ])
                    | '.' -> (splits, x :: newPos)
                    | _ -> failwith "unknown grid value")
                (splits, [])
                positions

        let distinct = List.distinct newPos
        findSplits splits distinct (y + 1)

let rec findPaths (paths: Map<int * int, int64>) y =
    if y >= height then
        Map.values paths |> Seq.sum
    else
        let newPaths =
            Map.fold
                (fun m (b, a) prevPaths ->
                    match grid[y][a] with
                    | '^' ->
                        m
                        |> Map.change (y, a - 1) (function
                            | Some v -> Some <| v + prevPaths
                            | None -> Some prevPaths)
                        |> Map.change (y, a + 1) (function
                            | Some v -> Some <| v + prevPaths
                            | None -> Some prevPaths)
                    | '.' ->
                        Map.change
                            (y, a)
                            (function
                            | Some v -> Some <| v + prevPaths
                            | None -> Some prevPaths)
                            m
                    | _ -> failwith "unknown grid value")
                Map.empty
                paths

        findPaths newPaths (y + 1)

let task1 = findSplits 0 [ startX ] 1
let task2 = findPaths (Map.add (0, startX) 1L Map.empty) 1

printfn $"Task 1: {task1}"
printfn $"Task 2: {task2}"
