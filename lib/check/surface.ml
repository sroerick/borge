(** Extract the public surface of an OCaml module.

    For modules with .mli files: the .mli is the authoritative surface.
    For modules without .mli: infer the surface from top-level bindings
    in the .ml file (heuristic — the real surface is what the compiler infers).

    Surface = { types, values, exceptions } where values are top-level
    let bindings and types are type/exception/module declarations. *)

type module_surface = {
  path : string;
  module_name : string;
  exports : string list;
  source : [ `Mli of string | `Ml_inferred of string ];
}

(** Check if a character is valid in an OCaml identifier *)
let is_ident_char c =
  (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') ||
  (c >= '0' && c <= '9') || c = '_' || c = '\''

(** Extract the name after 'let' in a top-level binding.
    Returns None if it's 'let ()', 'let _', or not at column 0. *)
let extract_toplevel_let_name line =
  let len = String.length line in
  if len < 4 then None
  else if String.sub line 0 4 <> "let " then None
  else begin
    let rest = String.sub line 4 (len - 4) in
    let rest_len = String.length rest in
    (* Skip 'rec ' *)
    let i = ref 0 in
    while !i + 3 < rest_len && String.sub rest !i 4 = "rec " do i := !i + 4 done;
    (* Skip spaces *)
    while !i < rest_len && rest.[!i] = ' ' do incr i done;
    if !i >= rest_len then None
    else begin
      let ch = rest.[!i] in
      (* let () = ... *)
      if ch = '(' && !i + 1 < rest_len && rest.[!i + 1] = ')' then None
      (* let _ = ... *)
      else if ch = '_' then None
      else begin
        let start = !i in
        while !i < rest_len && is_ident_char rest.[!i] do incr i done;
        if !i > start then Some (String.sub rest start (!i - start))
        else None
      end
    end
  end

(** Extract the name from 'type t = ...' or 'type name = ...' *)
let extract_type_name line =
  let len = String.length line in
  if len < 5 then None
  else if String.sub line 0 5 <> "type " then None
  else begin
    let rest = String.sub line 5 (len - 5) in
    let i = ref 0 in
    let rest_len = String.length rest in
    (* Skip 'nonrec ' *)
    while !i + 6 < rest_len && String.sub rest !i 7 = "nonrec " do i := !i + 7 done;
    while !i < rest_len && rest.[!i] = ' ' do incr i done;
    (* Skip 'rec ' (type rec doesn't exist but be safe) *)
    if !i >= rest_len then None
    else if rest.[!i] = '\'' then begin
      (* 'a t — type parameter, skip to the actual name *)
      while !i < rest_len && rest.[!i] <> ' ' do incr i done;
      while !i < rest_len && rest.[!i] = ' ' do incr i done;
      if !i >= rest_len then None
      else begin
        let start = !i in
        while !i < rest_len && is_ident_char rest.[!i] do incr i done;
        if !i > start then Some (String.sub rest start (!i - start))
        else None
      end
    end
    else begin
      let start = !i in
      while !i < rest_len && is_ident_char rest.[!i] do incr i done;
      if !i > start then Some (String.sub rest start (!i - start))
      else None
    end
  end

(** Extract the name from 'exception E ...' *)
let extract_exception_name line =
  let len = String.length line in
  if len < 10 then None
  else if String.sub line 0 10 <> "exception " then None
  else begin
    let rest = String.sub line 10 (len - 10) in
    let i = ref 0 in
    let rest_len = String.length rest in
    while !i < rest_len && rest.[!i] = ' ' do incr i done;
    if !i >= rest_len then None
    else begin
      let start = !i in
      while !i < rest_len && is_ident_char rest.[!i] do incr i done;
      if !i > start then Some (String.sub rest start (!i - start))
      else None
    end
  end

(** Extract the name from 'module M ...' *)
let extract_module_name line =
  let len = String.length line in
  if len < 7 then None
  else if String.sub line 0 7 <> "module " then None
  else begin
    let rest = String.sub line 7 (len - 7) in
    let i = ref 0 in
    let rest_len = String.length rest in
    (* Skip 'type ' (module type ...) *)
    if rest_len > 5 && String.sub rest 0 5 = "type " then None
    else begin
      (* Skip 'rec ' *)
      while !i + 3 < rest_len && String.sub rest !i 4 = "rec " do i := !i + 4 done;
      while !i < rest_len && rest.[!i] = ' ' do incr i done;
      if !i >= rest_len then None
      else begin
        let ch = rest.[!i] in
        if ch >= 'A' && ch <= 'Z' then begin
          let start = !i in
          while !i < rest_len && is_ident_char rest.[!i] do incr i done;
          Some (String.sub rest start (!i - start))
        end
        else None
      end
    end
  end

(** Extract all exports from an .ml or .mli file.
    Only looks at column 0 (top-level) declarations. *)
let extract_exports content =
  let lines = String.split_on_char '\n' content in
  let names = ref [] in
  List.iter (fun line ->
    match extract_toplevel_let_name line with
    | Some n -> names := n :: !names
    | None ->
      match extract_type_name line with
      | Some n -> names := n :: !names
      | None ->
        match extract_exception_name line with
        | Some n -> names := n :: !names
        | None ->
          match extract_module_name line with
          | Some n -> names := n :: !names
          | None -> ()
  ) lines;
  List.sort String.compare (List.sort_uniq String.compare !names)

(** Extract the surface of a single module given its .ml path.
    If a .mli exists, use that instead (it's authoritative). *)
let extract_surface ml_path =
  let mli_path = Filename.chop_extension ml_path ^ ".mli" in
  let module_name = String.capitalize_ascii
    (Filename.chop_extension (Filename.basename ml_path)) in
  if Sys.file_exists mli_path then
    let content = File_utils.read_file mli_path in
    { path = mli_path; module_name; exports = extract_exports content;
      source = `Mli mli_path }
  else
    let content = File_utils.read_file ml_path in
    { path = ml_path; module_name; exports = extract_exports content;
      source = `Ml_inferred ml_path }

(** Find .ml files in a directory *)
let find_ml_files dir =
  let rec find path =
    try
      let entries = Sys.readdir path in
      Array.fold_left (fun acc entry ->
        if entry = "_build" || entry = ".git" then acc
        else
          let full = Filename.concat path entry in
          if Sys.is_directory full then find full @ acc
          else if Filename.check_suffix entry ".ml" then full :: acc
          else acc
      ) [] entries
    with Sys_error _ -> []
  in
  List.sort String.compare (find dir)

(** Extract surfaces for all modules listed in a dune library. *)
let surfaces_for_library ml_files lib_modules =
  List.filter_map (fun mod_name ->
    let capitalized = String.capitalize_ascii mod_name in
    List.find_opt (fun path ->
      String.capitalize_ascii
        (Filename.chop_extension (Filename.basename path)) = capitalized
    ) ml_files
    |> Option.map (fun ml_path -> extract_surface ml_path)
  ) lib_modules
