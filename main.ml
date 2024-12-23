open Llbc
open Typing

let usage() =
  print_string "usage:\nllbc (-i in_file | in_file) [-s steps] [-c] [-t | --terminal-output]\n"
;;
    

let main () =
  let latex=ref true and chan=ref stdin and print_lvl= ref 0 
  and compile_latex = ref compile_latex_default and steps=ref steps_default 
  and inchan_name = ref None in
  let i=ref 0 in
  while !i +1 < (Array.length Sys.argv) do
    i:= !i +1;
    match Sys.argv.(!i) with
      | "--terminal-output" 
      | "-t" ->
	latex:=false
      | "-c" ->
	compile_latex:=true
      | "-l" ->
	i:= !i +1;
	if !i >= (Array.length Sys.argv) then begin
          usage();
          exit 1;
	end;
	print_lvl:= int_of_string (Sys.argv.(!i));
      | "-i" ->
	i:= !i +1;
	if !i >= (Array.length Sys.argv) then begin
          usage();
          exit 1;
	end;
        inchan_name:= Some(Sys.argv.(!i));
      | "-s" ->
	i:= !i +1;
	if !i >= (Array.length Sys.argv) then begin
          usage();
          exit 1;
	end;
        steps:= int_of_string(Sys.argv.(!i))
      | s -> inchan_name:= Some(s);
  done;
  begin match !inchan_name with
    | None -> usage(); exit 1
    | Some(name) -> chan:= open_in name
  end;
  let t=ref (Term_p(Ep)) in
  begin
  try
    let lexbuf = Lexing.from_channel !chan in
    t:= (Parser.input Lexer.token lexbuf);
    initialize_xu_index !t;
    (*
    output_tikz_of_term ~draw_dummy:false ~compile_latex:!compile_latex !t;
    *)
    strategy2 ~latex:(!latex) ~print_lvl:!print_lvl ~compile_latex:!compile_latex ~steps:!steps (!t);
  with Empty | End_of_file -> 
    print_string "Error: empty input\n";
    exit 0;
  end



      
let _ = main ()
