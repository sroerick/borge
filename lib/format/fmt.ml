open Borge_lang

type fmt_result =
  | Formatted of string
  | CheckClean
  | CheckDirty of string

(* agent note (|
 *   WHAT: Format a borge file from a string, returning the
 *   pretty-printed result.
 *
 *   WHY: Core formatting entry point - parses string input and
 *   re-serializes with proper formatting.
 * |) *)
let format_string input =
  let file = Parse.parse input in
  Print.print_file file

(* agent note (|
 *   WHAT: Format a borge file at the given path, returning the
 *   pretty-printed result wrapped in Formatted.
 *
 *   WHY: CLI entry point for formatting files.
 * |) *)
let format_file path =
  let input = File_utils.read_file path in
  Formatted (format_string input)

(* agent note (|
 *   WHAT: Remove trailing newline characters from a string.
 *
 *   WHY: Used for comparing formatted vs original content
 *   to check if formatting changed anything.
 * |) *)
let strip_trailing_newlines s =
  let len = String.length s in
  let i = ref (len - 1) in
  while !i >= 0 && s.[!i] = '\n' do decr i done;
  String.sub s 0 (!i + 1)

(* agent note (|
 *   WHAT: Check if a file would be changed by formatting.
 *   Returns CheckClean if identical, CheckDirty with formatted
 *   content if different.
 *
 *   WHY: Used by borge fmt --check to verify files are formatted.
 * |) *)
let check_file path =
  let input = File_utils.read_file path in
  let formatted = format_string input in
  if strip_trailing_newlines input = strip_trailing_newlines formatted then CheckClean
  else CheckDirty formatted
