(*arch -x86_64 zsh*)
(*ocamlc -o mycc ast.ml asm.ml lexer.ml parser.ml tacky.ml tackygen.ml codegen.ml emit.ml driver.ml*)

open Printf
open Lexer
open Parser
open Codegen
open Emit

let fail msg =
  eprintf "error: %s\n" msg;
  exit 1

let run_cmd cmd =
  let status = Sys.command cmd in
  if status <> 0 then
    fail ("command failed: " ^ cmd)

let remove_if_exists path =
  if Sys.file_exists path then Sys.remove path

let read_file path =
  let ic = open_in path in
  let len = in_channel_length ic in
  let s = really_input_string ic len in
  close_in ic;
  s

let write_file path contents =
  let oc = open_out path in
  output_string oc contents;
  close_out oc

type stage =
  | Lex
  | Parse
  | Codegen
  | Asm
  | Full
  | Tackygen

let preprocess input_file preprocessed_file =
  let cmd = sprintf "gcc -E -P %s -o %s" input_file preprocessed_file in
  run_cmd cmd

let assemble_and_link asm_file output_file =
  let cmd = sprintf "gcc %s -o %s" asm_file output_file in
  run_cmd cmd

let parse_args () =
  let stage = ref Full in
  let input_file = ref None in

  let set_stage s =
    match !stage with
    | Full -> stage := s
    | _ -> fail "multiple stage options are not allowed"
  in

  let rec loop i =
    if i >= Array.length Sys.argv then ()
    else
      match Sys.argv.(i) with
      | "--lex" -> set_stage Lex; loop (i + 1)
      | "--parse" -> set_stage Parse; loop (i + 1)
      | "--codegen" -> set_stage Codegen; loop (i + 1)
      | "--tacky" -> set_stage Tackygen; loop (i + 1)
      | "-S" -> set_stage Asm; loop (i + 1)
      | arg when String.length arg > 0 && arg.[0] = '-' ->
          fail ("unknown option: " ^ arg)
      | file ->
          if !input_file <> None then
            fail "only one input file allowed"
          else (
            input_file := Some file;
            loop (i + 1)
          )
  in
  loop 1;
  match !input_file with
  | None -> fail "no input file provided"
  | Some file -> (!stage, file)

let () =
  let stage, input_file = parse_args () in

  if not (Filename.check_suffix input_file ".c") then
    fail "input file must have a .c extension";

  let base = Filename.remove_extension input_file in
  let preprocessed = base ^ ".i" in
  let asm_file = base ^ ".s" in
  let output_file = base in

  remove_if_exists asm_file;
  remove_if_exists output_file;

  preprocess input_file preprocessed;
  let source = read_file preprocessed in
  remove_if_exists preprocessed;

  match stage with
  | Lex ->
      let _ = Lexer.lex source in
      ()
  | Parse ->
      let tokens = Lexer.lex source in
      let ast = Parser.parse tokens in
      print_endline (Ast.pp_program ast)
  | Codegen ->
      let tokens = Lexer.lex source in
      let ast = Parser.parse tokens in
      let _ = Codegen.gen_program ast in
      ()
  | Tackygen ->
      let tokens = Lexer.lex source in
      let ast = Parser.parse tokens in
      let tacky = Tackygen.gen_program ast in
      print_endline (Tacky.pp_program tacky)
  | Asm ->
      let tokens = Lexer.lex source in
      let ast = Parser.parse tokens in
      let asm_ast = Codegen.gen_program ast in
      let asm_text = Emit.emit_program asm_ast in
      write_file asm_file asm_text
  | Full ->
      let tokens = Lexer.lex source in
      let ast = Parser.parse tokens in
      let asm_ast = Codegen.gen_program ast in
      let asm_text = Emit.emit_program asm_ast in
      write_file asm_file asm_text;
      assemble_and_link asm_file output_file;
      remove_if_exists asm_file
