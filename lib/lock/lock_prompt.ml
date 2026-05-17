(* Prompt builder for agent sessions.

    Assembles structured prompts from the spec, convention rules,
    module surfaces, and role declarations.

    Does NOT depend on Borge_lib internals to avoid cyclic deps.
    All data is passed in via function arguments. *)

type role =
  | Implementer
  | Spec_writer
  | Observer
  | Fixer

let role_name = function
  | Implementer -> "implementer"
  | Spec_writer -> "spec-writer"
  | Observer -> "observer"
  | Fixer -> "fixer"

let role_description = function
  | Implementer ->
    "You are an implementer. Your job is to make the code match the spec. \\n     You may write code files and tests. You may NOT modify .borg files \\n     directly. You may NOT delete human-authored comments."
  | Spec_writer ->
    "You are a spec-writer. Your job is to edit the spec to describe \\n     what the code should be. You may modify .borg files directly. \\n     You may NOT write code files."
  | Observer ->
    "You are an observer. Your job is to check whether the code matches \\n     the spec. You may create findings and add comments. You may NOT write \\n     code files or modify .borg files."
  | Fixer ->
    "You are a fixer. Your job is to repair drift between code and spec. \\n     You may write code repairs and tests. You may NOT modify .borg files."

let make_prompt (role : role) ~(convention : string)
    ~(spec_files : (string * string) list)
    ~(module_surfaces : (string * string list) list) : string =
  let buf = Buffer.create 4096 in

  (* Role declaration *)
  Buffer.add_string buf (role_description role);
  Buffer.add_string buf "\n\n";

  (* Project convention *)
  Buffer.add_string buf (Printf.sprintf "Convention: %s\n\n" convention);

  (* Spec files as context *)
  Buffer.add_string buf "## SPEC FILES ##\n\n";
  List.iter (fun (path, content) ->
    Buffer.add_string buf (Printf.sprintf "### %s ###\n" path);
    Buffer.add_string buf content;
    Buffer.add_string buf "\n\n"
  ) spec_files;

  (* Module surfaces for context *)
  if module_surfaces <> [] then begin
    Buffer.add_string buf "## EXISTING MODULES ##\n\n";
    List.iter (fun (mod_name, exports) ->
      if exports <> [] then
        Buffer.add_string buf (Printf.sprintf "%s exports: %s\n"
          mod_name (String.concat ", " exports))
    ) module_surfaces;
    Buffer.add_string buf "\n"
  end;

  Buffer.contents buf
