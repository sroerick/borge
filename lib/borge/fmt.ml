open Borge_sexp

type fmt_result =
  | Formatted of string
  | CheckClean
  | CheckDirty of string

let format_string input =
  let file = Parse.parse input in
  Print.print_file file

let format_file path =
  let input = File_utils.read_file path in
  Formatted (format_string input)

let strip_trailing_newlines s =
  let len = String.length s in
  let i = ref (len - 1) in
  while !i >= 0 && s.[!i] = '\n' do decr i done;
  String.sub s 0 (!i + 1)

let check_file path =
  let input = File_utils.read_file path in
  let formatted = format_string input in
  if strip_trailing_newlines input = strip_trailing_newlines formatted then CheckClean
  else CheckDirty formatted
