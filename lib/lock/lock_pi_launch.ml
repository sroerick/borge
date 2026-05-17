(* Launch pi interactively with a structured prompt.
   Returns the exit status of pi. *)
let launch (prompt_path : string) : int =
  let cmd = Printf.sprintf "pi @%s" prompt_path in
  (* Direct invocation — pi is interactive, so backgrounding loses status.
     Future: detect tmux and open new window, detect ghostty and open new tab. *)
  Sys.command cmd
