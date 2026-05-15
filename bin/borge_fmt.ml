let read_file path =
  let ic = open_in path in
  let n = in_channel_length ic in
  let buf = Bytes.create n in
  really_input ic buf 0 n;
  close_in ic;
  Bytes.to_string buf

let run infile outfile =
  let input = read_file infile in
  let file = Borge_sexp.Parse.parse input in
  let txt = Borge_sexp.Print.print_file file in
  (match outfile with
   | Some dst ->
       let oc = open_out dst in
       output_string oc txt;
       close_out oc;
       Printf.printf "%s\n" (Filename.basename dst)
   | None -> print_endline txt);
  exit 0

let () =
  let args = Array.to_list Sys.argv in
  let rec parse_args args outfile =
    match args with
    | "--" :: rest -> (match rest with
                        | [f] -> (f, outfile)
                        | _ -> (List.hd args, outfile))
    | "-o" :: f :: rest -> parse_args rest (Some f)
    | "--o" :: f :: rest -> parse_args rest (Some f)
    | [] -> ("-stdin", outfile)
    | [f] -> (f, outfile)
    | _::_ -> ("-stdin", outfile)
  in
  let infile, outfile = parse_args (List.tl args) None in
  let infile = if infile = "-stdin" then "/dev/stdin" else infile in
  run infile outfile
