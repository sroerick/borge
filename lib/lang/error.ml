type parse_error = {
  message : string;
  line : int;
  column : int;
}

exception Parse_error of parse_error

(* agent note (|
 *   WHAT: Raise a parse error with the given line, column, and
 *   formatted message. Uses Format.kasprintf for printf-style formatting.
 *
 *   WHY: Parse errors need to be reported with location information
 *   so users know where in the file the problem occurred.
 * |) *)
let error ~line ~column fmt =
  Format.kasprintf
    (fun message -> raise (Parse_error { message; line; column }))
    fmt
