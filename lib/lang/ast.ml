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
}
