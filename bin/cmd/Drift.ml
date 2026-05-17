open Borge_lib

let run_agent dir =
  Printf.printf "Running agent drift analysis for '%s'...\n\n" dir;
  let (meta, agent_findings) = Agent.run_agent dir in
  Printf.printf "Static findings: %d\n" (List.length (List.filter (fun (f : Meta.finding) -> f.Meta.source = Meta.Static) meta.Meta.findings));
  Printf.printf "Agent findings: %d\n" (List.length agent_findings);
  Printf.printf "Total findings: %d\n" (List.length meta.Meta.findings);
  List.iter (fun (f : Meta.finding) ->
    Printf.printf "  [%s/%s] %s\n"
      (Meta.string_of_source f.Meta.source)
      (Meta.string_of_confidence f.Meta.confidence)
      (match f.Meta.detail with Some d -> d | None -> Meta.string_of_finding_type f.Meta.ft_type)
  ) meta.Meta.findings;
  Printf.printf "\nDetails written to .borg.meta files.\n";
  exit 0

let run dir agent json =
  if agent then run_agent dir
  else begin
    (* Generate and write .borg.meta files *)
    let _meta = Drift.write_meta_files dir in
    let result = Drift.run dir in
    if json then begin
      Printf.printf "%s\n" (Yojson.Basic.to_string (Json_out.drift result));
      let total = List.length result.spec_drift + List.length result.code_drift +
                  List.length result.structural_drift in
      exit (if total > 0 then 1 else 0)
    end;
    Printf.printf "Drift report for '%s'\n\n" dir;
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
        | "unspecified-module" ->
            Printf.printf "  ▷ %s: %s (unspecified-module)\n" d.path d.name
        | "unspecified-file" ->
            Printf.printf "  ▷ %s: .ml file not mentioned in any .borg spec\n" d.path
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
    Printf.printf "Details written to .borg.meta files.\n";
    if total > 0 then exit 1 else exit 0
  end

open Cmdliner

let dir =
  Arg.(value & pos 0 dir "." & info [] ~docv:"DIR"
    ~doc:"Directory to check for drift")

let agent =
  Arg.(value & flag & info ["agent"] ~doc:
    "Run LLM-powered semantic drift analysis on top of static analysis")

let json =
  Arg.(value & flag & info ["json"] ~doc:"Output as JSON")

let cmd : unit Cmd.t =
  Cmd.v (Cmd.info "drift" ~doc:"detect spec, code, and structural drift"
    ~man:[`S "DESCRIPTION";
          `P "Checks for three layers of drift:";
          `P "1. Spec drift: .borg describes something not in the code";
          `P "2. Code drift: code has something not in any .borg spec";
          `P "3. Structural drift: code organization violates the convention";
          `S "OPTIONS";
          `P "With --agent, runs LLM semantic analysis after static checks.";
          `P "With --json, outputs structured JSON instead of formatted text."])
  Term.(const run $ dir $ agent $ json)
