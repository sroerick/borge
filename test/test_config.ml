(* Test config: .borgerc parsing, merge precedence, api-key resolution. *)
open Printf
open Borge_lib

(* Helper: write a string to a temp path. *)
let write_temp path content =
  let oc = open_out path in
  output_string oc content;
  close_out oc

let test_parse_basic () =
  let tmp = "/tmp/borge_config_basic.borgerc" in
  write_temp tmp {|(borge-config
  (developer-name "roerick")
  (api-keys
    (anthropic (env ANTHROPIC_API_KEY))
    (openai (value "sk-test123"))))
|};
  (match Config.load_from_path tmp with
   | None -> Alcotest.fail "load returned None"
   | Some c ->
       Alcotest.(check string) "developer-name" "roerick" (Option.value c.developer_name ~default:"");
       Alcotest.(check int) "2 api keys" 2 (List.length c.api_keys);
       (* keys are sorted by provider name on parse *)
       (match List.assoc_opt "anthropic" c.api_keys with
        | Some (Config.Env v) -> Alcotest.(check string) "env var name" "ANTHROPIC_API_KEY" v
        | _ -> Alcotest.fail "anthropic key not Env");
       (match List.assoc_opt "openai" c.api_keys with
        | Some (Config.Literal v) -> Alcotest.(check string) "literal key" "sk-test123" v
        | _ -> Alcotest.fail "openai key not Literal"));
  Sys.command (sprintf "rm -f %s" tmp) |> ignore

let test_developer_name_default () =
  (* An empty borge-config has no developer-name; accessor returns "agent". *)
  let tmp = "/tmp/borge_config_empty.borgerc" in
  write_temp tmp "(borge-config\n)\n";
  (match Config.load_from_path tmp with
   | Some c ->
       Alcotest.(check string) "default name when unset" "agent" (Config.developer_name c)
   | None -> Alcotest.fail "load returned None for empty config");
  Sys.command (sprintf "rm -f %s" tmp) |> ignore

let test_merge_precedence () =
  (* Project overrides user: developer-name project wins; overlapping
     api-key provider project wins; user-only provider preserved. *)
  let project = {
    Config.developer_name = Some "project-dev";
    api_keys = [("anthropic", Config.Env "PROJECT_ANTHROPIC")];
  } in
  let user = {
    Config.developer_name = Some "user-dev";
    api_keys = [
      ("anthropic", Config.Env "USER_ANTHROPIC");
      ("openai", Config.Literal "user-openai-key");
    ];
  } in
  let merged = Config.merge ~project ~user in
  Alcotest.(check string) "project developer_name wins" "project-dev" (Option.value merged.developer_name ~default:"");
  Alcotest.(check int) "2 keys after union" 2 (List.length merged.api_keys);
  (match List.assoc_opt "anthropic" merged.api_keys with
   | Some (Config.Env v) -> Alcotest.(check string) "project anthropic wins" "PROJECT_ANTHROPIC" v
   | _ -> Alcotest.fail "merged anthropic missing");
  (match List.assoc_opt "openai" merged.api_keys with
   | Some _ -> ()  (* user-only key preserved *)
   | None -> Alcotest.fail "user openai key lost in merge")

let test_merge_user_only_name () =
  (* When project has no developer-name, user's is used. *)
  let project = { Config.developer_name = None; api_keys = [] } in
  let user = { Config.developer_name = Some "user-dev"; api_keys = [] } in
  let merged = Config.merge ~project ~user in
  Alcotest.(check string) "user dev name used when project unset" "user-dev" (Option.value merged.developer_name ~default:"")

let test_resolve_api_key_env () =
  (* resolve_api_key with (env X) reads the env var. *)
  Unix.putenv "BORGE_TEST_KEY" "secret-value-123";
  let c = {
    Config.developer_name = None;
    api_keys = [("test", Config.Env "BORGE_TEST_KEY")];
  } in
  (match Config.resolve_api_key c "test" with
   | Some v -> Alcotest.(check string) "env var resolved" "secret-value-123" v
   | None -> Alcotest.fail "env var not resolved")

let test_resolve_api_key_missing () =
  (* resolve_api_key for an unknown provider returns None. *)
  let c = { Config.developer_name = None; api_keys = [] } in
  (match Config.resolve_api_key c "nonexistent" with
   | None -> ()  (* expected *)
   | Some _ -> Alcotest.fail "expected None for unknown provider")

let test_parse_missing_file () =
  (* A missing .borgerc returns None, doesn't crash. *)
  Alcotest.(check bool) "missing file -> None" true (Config.load_from_path "/tmp/borge_config_does_not_exist_xyz" = None)

let () =
  Alcotest.run "config" [
    "parse", [
      "basic parse", `Quick, test_parse_basic;
      "developer-name default", `Quick, test_developer_name_default;
      "missing file returns None", `Quick, test_parse_missing_file;
    ];
    "merge", [
      "project overrides user", `Quick, test_merge_precedence;
      "user-only developer name", `Quick, test_merge_user_only_name;
    ];
    "resolve", [
      "env api-key resolved", `Quick, test_resolve_api_key_env;
      "unknown provider None", `Quick, test_resolve_api_key_missing;
    ];
  ]
