let input = __SOURCE_DIRECTORY__ + @"/input.txt"
type Point = float * float
type InputSet = { gopher : Point; dog : Point; holes : Point list }

let parseInput file =
  file |> System.IO.File.ReadAllLines |> Seq.filter (fun l -> not <| System.String.IsNullOrEmpty l )
  |> Seq.map (fun l -> l.Split(' ') |> Array.map float)
  |> Seq.fold (fun sets ls -> match sets, ls with 
                              | _, [| _; x1; y1; x2; y2 |] -> {gopher = (x1, y1); dog = (x2, y2); holes = []} :: sets
                              | (h::t, [| x; y|])          -> {h with holes = (x,y) :: h.holes} :: t
                              | _                          -> failwith "invalid input") []
  |> Seq.rev 
  |> Seq.map (fun p -> {p with holes = (List.rev p.holes)})

let distance (x1, y1) (x2, y2) =
  let difference x' y' = (x' - y') * (x' - y')
  (difference x2 x1) + (difference y2 y1) |> System.Math.Sqrt

let findSafeHole data =
  let gDist = distance data.gopher
  let dDist = distance data.dog
  data.holes |> Seq.tryFind (fun h -> 2. * (gDist h) < (dDist h))

let printResult = function Some p -> printfn "The gopher can escape through the hole at %A" p
                         | _      -> printfn "The gopher cannot escape."

let simulate : string -> unit = parseInput >> Seq.map findSafeHole >> Seq.iter printResult
simulate input