open Borge_lib

let run dir =
  Printf.printf "Drift report for '%s'\n\n" dir;
  let result = Drift.run dir in
  (* Spec drift *)
  if result.spec_drift = [] then
    Printf.printf "Spec drift: none\n"
  else begin
    Printf.printf "Spec drift (%d sections):\n" (List.length result.spec_drift);
    List.iter (fun (d : Drift.spec_drift) ->
      Printf.printf "  ▷ %s: '%s' marked implemented but not found in code\n"
        d.path d.section_name
    ) result.spec_drift
  end;
  (* Code drift *)
  if result.code_drift = [] then
    Printf.printf "Code drift: none\n"
  else begin
    Printf.printf "Code drift (%d items):\n" (List.length result.code_drift);
    List.iter (fun (d : Drift.code_drift_item) ->
      match d.kind with
      | "unspecified-file" ->
          Printf.printf "  ▷ %s: .ml file not mentioned in any .borg spec\n" d.path
      | "undocumented-binding" ->
          Printf.printf "  ▷ %s: binding '%s' has no borge-style documentation comment\n"
            d.path d.name
      | _ ->
          Printf.printf "  ▷ %s: %s (%s)\n" d.path d.name d.kind
    ) result.code_drift
  end;
  (* Structural drift *)
  if result.structural_drift = [] then
    Printf.printf "Structural drift: none\n"
  else begin
    Printf.printf "Structural drift (%d items):\n" (List.length result.structural_drift);
    List.iter (fun (d : Drift.structural_drift_item) ->
      Printf.printf "  ▷ %s\n" d.description
    ) result.structural_drift
  end;
  let total = List.length result.spec_drift + List.length result.code_drift +
              List.length result.structural_drift in
  Printf.printf "\nTotal drift: %d item(s)\n" total;
  if total > 0 then exit 1 else exit 0

open Cmdliner

let dir =
  Arg.(value & pos 0 dir "." & info [] ~docv:"DIR"
    ~doc:"Directory to check for drift")

let cmd =
  Cmd.v (Cmd.info "drift" ~doc:"detect spec, code, and structural drift"
    ~man:[`S "DESCRIPTION";
          `P "Checks for three layers of drift:";
          `P "1. Spec drift: .borg describes something not in the code";
          `P "2. Code drift: code has something not in any .borg spec";
          `P "3. Structural drift: code organization violates the convention"])
  Term.(const run $ dir)

let () = ignore (Cmd.eval cmd : int)
