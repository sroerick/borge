(** Read, merge, and expose borge configuration.

    Reads .borgerc files (sexp format, same parser as .borg) and exposes
    a typed config record. Project ./.borgerc overrides user
    ~/.config/borge/config. Spec in lib.borg `config-core` and
    docs/engine.borg `config-file`.

    Config file format:
      (borge-config
        (developer-name "roerick")
        (api-keys
          (anthropic (env ANTHROPIC_API_KEY))
          (openai (value "sk-..."))))

    Keys:
    - developer-name: author name for annotated comments. Defaults to
      "agent" when unset (mirrors the pre-config behavior of lock-prompt,
      lint, comment-generating code).
    - api-keys: provider→source map. Each source is (env VAR) or
      (value "literal"). resolve_api_key looks up by provider name and
      resolves env indirection.

    Loading: project merged over user. Project values win on conflict;
    api-keys are unioned (project keys override user keys with the same
    provider name). *)

open Printf

(** {1 Types} *)

type api_key_source =
  | Env of string      (* (env VAR_NAME) — read from environment *)
  | Literal of string  (* (value "literal-key") — literal string *)

type config = {
  developer_name : string option;
  api_keys : (string * api_key_source) list;
}

(** {1 Defaults} *)

let empty = { developer_name = None; api_keys = [] }

let default_developer_name = "agent"

let default () = { developer_name = None; api_keys = [] }

(** {1 Paths} *)

(* Project config: ./.borgerc at CWD (checked into git, shared by team). *)
let project_config_path () = ".borgerc"

(* User config: ~/.config/borge/config (personal, not in git). *)
let user_config_path () =
  match Sys.getenv_opt "HOME" with
  | Some home -> Filename.concat (Filename.concat home ".config/borge") "config"
  | None -> Filename.concat ".config/borge" "config"

(** {1 Parsing} *)

(* Extract raw string content from a string_value, regardless of form. *)
let string_content = function
  | Borge_lang.Ast.String (_, Borge_lang.Ast.Quoted q) -> Some q.q_content
  | Borge_lang.Ast.String (_, Borge_lang.Ast.Verbatim v) -> Some v.v_content
  | _ -> None

(* Extract the value of a leaf sexp: atom or quoted/verbatim string. *)
let value_of_sexp (s : Borge_lang.Ast.sexp) : string option =
  match s with
  | Borge_lang.Ast.Atom (_, a) -> Some a
  | Borge_lang.Ast.String _ -> string_content s
  | _ -> None

(* Parse an api-key source: (env VAR) or (value "literal"). *)
let parse_key_source : Borge_lang.Ast.sexp -> api_key_source option = function
  | Borge_lang.Ast.List (_, [Borge_lang.Ast.Atom (_, "env"); arg]) ->
      (match value_of_sexp arg with
       | Some v -> Some (Env v)
       | None -> None)
  | Borge_lang.Ast.List (_, [Borge_lang.Ast.Atom (_, "value"); arg]) ->
      (match value_of_sexp arg with
       | Some v -> Some (Literal v)
       | None -> None)
  | _ -> None

(* Parse the (api-keys ...) block: each child is (PROVIDER SOURCE). *)
let parse_api_keys (node : Borge_lang.Ast.sexp) : (string * api_key_source) list =
  match node with
  | Borge_lang.Ast.List (_, Borge_lang.Ast.Atom (_, "api-keys") :: children) ->
      List.filter_map (fun child ->
        match child with
        | Borge_lang.Ast.List (_, [provider; src_node]) ->
            (match value_of_sexp provider, parse_key_source src_node with
             | Some name, Some src -> Some (name, src)
             | _ -> None)
        | _ -> None
      ) children
  | _ -> []

(* Parse a top-level (borge-config ...) form into a config record. *)
let parse_config (text : string) : config option =
  let file = Borge_lang.Parse.parse_file text in
  let rec walk = function
    | [] -> None
    | swc :: rest ->
        (match swc.Borge_lang.Ast.node with
         | Borge_lang.Ast.List (_, Borge_lang.Ast.Atom (_, "borge-config") :: fields) ->
             let dev_name = ref None in
             let api_keys = ref [] in
             List.iter (fun f ->
               match f with
               | Borge_lang.Ast.List (_, Borge_lang.Ast.Atom (_, "developer-name") :: [v]) ->
                   dev_name := value_of_sexp v
               | Borge_lang.Ast.List (_, Borge_lang.Ast.Atom (_, "api-keys") :: _)
                   as api_node ->
                   api_keys := parse_api_keys api_node
               | _ -> ()
             ) fields;
             Some { developer_name = !dev_name; api_keys = !api_keys }
         | _ -> walk rest)
  in
  walk file.Borge_lang.Ast.top_level

(** {1 Loaders} *)

let load_from_path path : config option =
  if not (Sys.file_exists path) then None
  else begin
    let ic = open_in path in
    let len = in_channel_length ic in
    let buf = Bytes.create len in
    really_input ic buf 0 len;
    close_in ic;
    parse_config (Bytes.to_string buf)
  end

let load_project_config () : config option =
  load_from_path (project_config_path ())

let load_user_config () : config option =
  load_from_path (user_config_path ())

(** {1 Merge}

    Project values override user values. For developer_name: project
    wins if set, else user's, else None. For api_keys: union, project
    entries override user entries with the same provider name. *)

let merge ~project ~user =
  let developer_name =
    match project.developer_name with
    | Some _ as v -> v
    | None -> user.developer_name
  in
  (* Build a name→source map. Insert user first, then project overwrites. *)
  let tbl = Hashtbl.create 8 in
  List.iter (fun (k, v) -> Hashtbl.replace tbl k v) user.api_keys;
  List.iter (fun (k, v) -> Hashtbl.replace tbl k v) project.api_keys;
  let api_keys =
    Hashtbl.fold (fun k v acc -> (k, v) :: acc) tbl []
    |> List.sort (fun (a, _) (b, _) -> String.compare a b)
  in
  { developer_name; api_keys }

let load_config () : config =
  let project = match load_project_config () with Some c -> c | None -> empty in
  let user = match load_user_config () with Some c -> c | None -> empty in
  merge ~project ~user

(** {1 Accessors} *)

let developer_name (c : config) : string =
  match c.developer_name with
  | Some n -> n
  | None -> default_developer_name

let resolve_api_key (c : config) (provider : string) : string option =
  let src =
    try Some (List.assoc provider c.api_keys) with Not_found -> None
  in
  match src with
  | None -> None
  | Some (Env var) -> Sys.getenv_opt var
  | Some (Literal lit) -> Some lit

(** {1 Format (for debugging / display) } *)

let string_of_api_key_source = function
  | Env v -> sprintf "(env %s)" v
  | Literal _ -> "(value <redacted>)"  (* don't print literal keys *)

let string_of_config (c : config) : string =
  let dev = match c.developer_name with
    | Some n -> n | None -> default_developer_name in
  let keys =
    List.map (fun (k, s) -> sprintf "  (%s %s)" k (string_of_api_key_source s))
      c.api_keys
    |> String.concat "\n"
  in
  sprintf "(borge-config\n  (developer-name %s)\n  (api-keys\n%s))" dev keys
