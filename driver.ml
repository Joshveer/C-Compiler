open Printf
open Lexer
open Parser
open Semanticanalysis
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
  | Validate
  | Codegen
  | Asm
  | Full
  | Tackygen

let preprocess input_file preprocessed_file =
  let cmd = sprintf "gcc -E -P %s -o %s" input_file preprocessed_file in
  run_cmd cmd

let assemble_and_link asm_file output_file =
  let cmd = sprintf "gcc -arch x86_64 %s -o %s" asm_file output_file in
  run_cmd cmd

let parse_args () =
  let stage = ref Full in
  let input_file = ref "" in
  let usage_msg = "Usage: ./mycc [options] <input_file>" in
  let set_stage s = Arg.Unit (fun () -> stage := s) in
  let speclist =
    [ ("--lex", set_stage Lex, "Run lexer only")
    ; ("--parse", set_stage Parse, "Run parser only")
    ; ("--validate", set_stage Validate, "Run semantic analysis only")
    ; ("--tacky", set_stage Tackygen, "Run Tacky generation only")
    ; ("--codegen", set_stage Codegen, "Run code generation only")
    ]
  in
  Arg.parse speclist (fun filename -> input_file := filename) usage_msg;
  if !input_file = "" then (
    Arg.usage speclist usage_msg;
    exit 1
  );
  (!stage, !input_file)

let () =
  let stage, input_file = parse_args () in
  let base = Filename.remove_extension input_file in
  let preprocessed = base ^ ".i" in
  let asm_file = base ^ ".s" in
  let output_file = base in

  remove_if_exists asm_file;
  remove_if_exists output_file;

  preprocess input_file preprocessed;
  let source = read_file preprocessed in
  remove_if_exists preprocessed;

  try
    match stage with
    | Lex ->
        let _ = Lexer.lex source in
        ()
    | Parse ->
        let tokens = Lexer.lex source in
        let ast = Parser.parse tokens in
        print_endline (Ast.pp_program ast)
    | Validate ->
        let tokens = Lexer.lex source in
        let ast = Parser.parse tokens in
        let _ = Semanticanalysis.resolve_program ast in
        ()
    | Tackygen ->
        let tokens = Lexer.lex source in
        let ast = Parser.parse tokens in
        let resolved_ast = Semanticanalysis.resolve_program ast in
        let tacky = Tackygen.gen_program resolved_ast in
        print_endline (Tacky.pp_program tacky)
    | Codegen ->
        let tokens = Lexer.lex source in
        let ast = Parser.parse tokens in
        let resolved_ast = Semanticanalysis.resolve_program ast in
        let tacky = Tackygen.gen_program resolved_ast in
        let _ = Codegen.gen_program tacky in
        ()
    | Asm ->
        let tokens = Lexer.lex source in
        let ast = Parser.parse tokens in
        let resolved_ast = Semanticanalysis.resolve_program ast in
        let tacky = Tackygen.gen_program resolved_ast in
        let asm_ast = Codegen.gen_program tacky in
        let asm_text = Emit.emit_program asm_ast in
        write_file asm_file asm_text
    | Full ->
        let tokens = Lexer.lex source in
        let ast = Parser.parse tokens in
        let resolved_ast = Semanticanalysis.resolve_program ast in
        let tacky = Tackygen.gen_program resolved_ast in
        let asm_ast = Codegen.gen_program tacky in
        let asm_text = Emit.emit_program asm_ast in
        write_file asm_file asm_text;
        assemble_and_link asm_file output_file
  with
  | Lexer.LexError msg -> fail ("Lexing error: " ^ msg)
  | Parser.ParseError msg -> fail ("Parsing error: " ^ msg)
  | Semanticanalysis.SemanticError msg -> fail ("Semantic error: " ^ msg)
  | e -> fail (Printexc.to_string e)
