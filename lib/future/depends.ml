(* Parse dependency chains - STUB *)

type dep_graph = { nodes : string list; edges : (string * string) list }

let empty_graph = { nodes = []; edges = [] }
let build_graph _dir = empty_graph
let topological_sort _graph = []
