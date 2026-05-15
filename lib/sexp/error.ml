type parse_error = {
  message : string;
  line : int;
  column : int;
}

exception Parse_error of parse_error

let error ~line ~column fmt =
  Format.kasprintf
    (fun message -> raise (Parse_error { message; line; column }))
    fmt
