let input = __SOURCE_DIRECTORY__ + @"/input.txt"
type Point = float * float
type InputSet = { gopher : Point; dog : Point; holes : Point list }

let parseInput file =
  let newSet [| _; x1; y1; x2; y2 |] = {gopher = (x1, y1); dog = (x2, y2); holes = []}
  file |> System.IO.File.ReadAllLines |> Seq.filter (fun l -> not <| System.String.IsNullOrEmpty l )
  |> Seq.fold (fun sets l -> l.Split(' ') |> Array.map float
                             |> function [| x; y |] -> let h::t = sets
                                                       {h with holes = ((x, y) :: h.holes)} :: t
                                       | pts -> (newSet pts) :: sets) []
  |> Seq.rev
  |> Seq.map (fun p -> {p with holes = p.holes |> List.rev})

let distance (x1, y1) (x2, y2) =
  let difference x' y' = (x' - y') * (x' - y')
  (difference x2 x1) + (difference y2 y1) |> System.Math.Sqrt

let findSafeHole data =
  let gDist = distance data.gopher
  let dDist = distance data.dog
  data.holes |> Seq.tryFind (fun h -> 2. * (gDist h) < (dDist h))

let printResult = function Some p -> printfn "The gopher can escape through the hole at %A" p
                         | _      -> printfn "The gopher cannot escape."

let simulate = parseInput >> Seq.map findSafeHole >> Seq.iter printResult
simulate input