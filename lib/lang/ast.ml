type pos = {
  line : int;
  col : int;
  offset : int;
}

type verbatim_string = {
  v_content : string;
  (* The raw text between (| and |), newlines preserved *)
}

type quoted_string = {
  q_content : string;
  (* The decoded text, escapes already resolved *)
}

type string_value =
  | Quoted of quoted_string
  | Verbatim of verbatim_string

type symbol = string

type sexp =
  | Atom of pos * symbol
  | String of pos * string_value
  | List of pos * sexp list

(** A plain (;) comment — not part of the AST structure,
    but preserved for round-tripping *)
type plain_comment = {
  text : string;
  line : int;
}

(** An annotated (* *) comment — first-class AST node *)
type authorship =
  | Single of symbol
  | Multiple of symbol list

type comment_type =
  | Untyped  (* two-position: (* author value *) — implicitly "note" *)
  | Typed of symbol  (* three-position: (* author type value *) *)

type annotated_comment = {
  authorship : authorship;
  comment_type : comment_type;
  value : string_value option;  (* None for empty (||) *)
  start_line : int;
  end_line : int;
}

(** Attachment: comments are attached to the next AST node *)
type comment_attachment =
  | Plain of plain_comment
  | Annotated of annotated_comment

type sexp_with_comments = {
  comments_before : comment_attachment list;
  node : sexp;
  end_pos : pos;  (* position after the closing delimiter *)
}

type file = {
  top_level_comments : comment_attachment list;
  (* Comments at the top of file before any sexp *)
  top_level : sexp_with_comments list;
  (* The main s-expressions in the file *)
  trailing_comments : comment_attachment list;
  (* Comments after the last sexp *)
  nested_comments : (pos * comment_attachment) list;
  (* Comments inside list forms, keyed by source position. Kept as
     side data because Ast.List currently stores bare sexp children
     without comment_attachment wrappers. See the comment-attachment
     section in sexp.borg. *)
}

(** Single traversal over every sexp in a file, depth-first,
    in source order. *)
let iter_sexps file f =
  let rec go_sexp s =
    f s;
    match s with
    | List (_, children) -> List.iter go_sexp children
    | _ -> ()
  and go_swc swc = go_sexp swc.node in
  List.iter go_swc file.top_level

(** Fold over every sexp in a file, depth-first, in source order. *)
let fold_sexps file acc0 f =
  let rec go_sexp acc s =
    match s with
    | List (_, children) -> List.fold_left go_sexp (f acc s) children
    | _ -> f acc s
  in
  let go_swc acc swc = go_sexp acc swc.node in
  List.fold_left go_swc acc0 file.top_level

(** Return all comments in source order: top-level leading, before each
    top-level node, nested inside list forms, trailing. *)
let all_comments file =
  file.top_level_comments
  @ List.concat_map (fun swc -> swc.comments_before) file.top_level
  @ List.map snd (List.sort (fun (p1, _) (p2, _) -> compare p1.offset p2.offset) file.nested_comments)
  @ file.trailing_comments

let iter_comments file f = List.iter f (all_comments file)

let fold_comments file acc0 f = List.fold_left f acc0 (all_comments file)

let start_pos_of = function
  | Atom (p, _) | String (p, _) | List (p, _) -> p

let rec end_pos_of = function
  | Atom (p, _) | String (p, _) | List (p, []) -> p
  | List (_, children) -> end_pos_of (List.hd (List.rev children))

let nested_comments_acc : (pos * comment_attachment) list ref = ref []
let reset_nested_comments () = nested_comments_acc := []
