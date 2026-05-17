open Borge_lang

type dune_library = {
  name : string;
  public_name : string option;
  modules : string list;
  libraries : string list;
}

type dune_executable = {
  names : string list;
  public_names : string list;
  libraries : string list;
}

type dune_test = {
  name : string;
  libraries : string list;
}

type dune_stanza =
  | Library of dune_library
  | Executables of dune_executable
  | Test of dune_test

type dune_file = {
  path : string;
  stanzas : dune_stanza list;
}

let dummy_pos = { Ast.line = 0; col = 0; offset = 0 }

(** Extract a string list from a sexp list like (modules a b c) *)
let extract_string_list = function
  | Ast.List (_, items) ->
      List.filter_map (function
        | Ast.Atom (_, s) -> Some s
        | _ -> None
      ) items
  | _ -> []

(** Extract an optional string value from (key value) *)
let extract_optional_string key children =
  List.find_map (function
    | Ast.List (_, Ast.Atom (_, k) :: Ast.Atom (_, v) :: _)
      when k = key -> Some v
    | _ -> None
  ) children

(** Extract a required string value from (key value) *)
let extract_string key children =
  extract_optional_string key children

(** Parse a (library ...) stanza *)
let parse_library children =
  let name = match extract_string "name" children with
    | Some n -> n
    | None -> ""
  in
  let public_name = extract_optional_string "public_name" children in
  let modules = List.find_map (function
    | Ast.List (_, Ast.Atom (_, "modules") :: rest) ->
        Some (extract_string_list (Ast.List (dummy_pos, rest)))
    | _ -> None
  ) children |> function Some m -> m | None -> [] in
  let libraries = List.find_map (function
    | Ast.List (_, Ast.Atom (_, "libraries") :: rest) ->
        Some (extract_string_list (Ast.List (dummy_pos, rest)))
    | _ -> None
  ) children |> function Some l -> l | None -> [] in
  { name; public_name; modules; libraries }

(** Parse an (executables ...) or (executable ...) stanza *)
let parse_executables children =
  let names = List.find_map (function
    | Ast.List (_, Ast.Atom (_, "name") :: Ast.Atom (_, n) :: _) ->
        Some [n]
    | Ast.List (_, Ast.Atom (_, "names") :: rest) ->
        Some (extract_string_list (Ast.List (dummy_pos, rest)))
    | _ -> None
  ) children |> function Some n -> n | None -> [] in
  let public_names = List.find_map (function
    | Ast.List (_, Ast.Atom (_, "public_name") :: Ast.Atom (_, n) :: _) ->
        Some [n]
    | Ast.List (_, Ast.Atom (_, "public_names") :: rest) ->
        Some (extract_string_list (Ast.List (dummy_pos, rest)))
    | _ -> None
  ) children |> function Some n -> n | None -> [] in
  let libraries = List.find_map (function
    | Ast.List (_, Ast.Atom (_, "libraries") :: rest) ->
        Some (extract_string_list (Ast.List (dummy_pos, rest)))
    | _ -> None
  ) children |> function Some l -> l | None -> [] in
  { names; public_names; libraries }

(** Parse a (test ...) stanza *)
let parse_test children =
  let name = match extract_string "name" children with
    | Some n -> n
    | None -> ""
  in
  let libraries = List.find_map (function
    | Ast.List (_, Ast.Atom (_, "libraries") :: rest) ->
        Some (extract_string_list (Ast.List (dummy_pos, rest)))
    | _ -> None
  ) children |> function Some l -> l | None -> [] in
  { name; libraries }

(** Parse a single dune file *)
let parse_dune_file path =
  let input = File_utils.read_file path in
  let file = Parse.parse_file input in
  let stanzas = List.filter_map (fun { Ast.node; _ } ->
    match node with
    | Ast.List (_, Ast.Atom (_, "library") :: children) ->
        Some (Library (parse_library children))
    | Ast.List (_, Ast.Atom (_, "executable") :: children)
    | Ast.List (_, Ast.Atom (_, "executables") :: children) ->
        Some (Executables (parse_executables children))
    | Ast.List (_, Ast.Atom (_, "test") :: children) ->
        Some (Test (parse_test children))
    | _ -> None
  ) file.Ast.top_level in
  { path; stanzas }

(** Find all dune files recursively *)
let find_dune_files dir =
  let rec find path =
    try
      let entries = Sys.readdir path in
      Array.fold_left (fun acc entry ->
        if entry = "_build" || entry = ".git" then acc
        else
          let full = Filename.concat path entry in
          if Sys.is_directory full then find full @ acc
          else if entry = "dune" then full :: acc
          else acc
      ) [] entries
    with Sys_error _ -> []
  in
  List.sort String.compare (find dir)

(** Parse all dune files in a project *)
let parse_all dir =
  List.map parse_dune_file (find_dune_files dir)

(** Get all library names across all dune files *)
let all_library_names dune_files =
  List.concat_map (fun { stanzas; _ } ->
    List.filter_map (function
      | Library lib -> Some lib.name
      | _ -> None
    ) stanzas
  ) dune_files

(** Get all modules in a specific library *)
let modules_in_library dune_files lib_name =
  List.find_map (fun { stanzas; _ } ->
    List.find_map (function
      | Library lib when lib.name = lib_name -> Some lib.modules
      | _ -> None
    ) stanzas
  ) dune_files |> function Some m -> m | None -> []

(** Get all modules across all libraries *)
let all_modules dune_files =
  List.concat_map (fun { stanzas; _ } ->
    List.concat_map (function
      | Library lib -> List.map (fun m -> (lib.name, m)) lib.modules
      | _ -> []
    ) stanzas
  ) dune_files
