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
  | Object
  | Full
  | Tackygen

let preprocess input_file preprocessed_file =
  let cmd = sprintf "gcc -E -P %s -o %s" input_file preprocessed_file in
  run_cmd cmd

let assemble asm_file object_file =
  let cmd = sprintf "gcc -arch x86_64 -c %s -o %s" asm_file object_file in
  run_cmd cmd

let assemble_and_link asm_file output_file =
  let cmd = sprintf "gcc -arch x86_64 %s -o %s" asm_file output_file in
  run_cmd cmd

let parse_args () =
  let stage = ref Full in
  let input_file = ref "" in
  let usage_msg = "Usage: ./mycc [options] <input_file>" in
  let set_stage s = Arg.Unit (fun () -> stage := s) in
  let speclist = [
    ("-c", set_stage Object, "Compile to object file");
    ("--lex", set_stage Lex, "Run lexer");
    ("--parse", set_stage Parse, "Run parser");
    ("--validate", set_stage Validate, "Run semantic analysis");
    ("--tacky", set_stage Tackygen, "Generate Tacky IR");
    ("--codegen", set_stage Codegen, "Generate Assembly IR");
    ("--asm", set_stage Asm, "Generate Assembly file");
    ("--stage", Arg.String (fun s ->
       match s with
       | "lex" -> stage := Lex
       | "parse" -> stage := Parse
       | "validate" -> stage := Validate
       | "tacky" -> stage := Tackygen
       | "codegen" -> stage := Codegen
       | "asm" -> stage := Asm
       | _ -> ()), "Set stage (compatibility)");
    ("--chapter", Arg.Int (fun _ -> ()), "Ignored");
  ] in
  Arg.parse speclist (fun filename -> input_file := filename) usage_msg;
  if !input_file = "" then (Arg.usage speclist usage_msg; exit 1);
  (!stage, !input_file)

let () =
  let stage, input_file = parse_args () in
  let base_name = Filename.remove_extension input_file in
  let preprocessed_file = base_name ^ ".i" in
  let asm_file = base_name ^ ".s" in
  let output_file = match stage with Object -> base_name ^ ".o" | _ -> "a.out" in

  try
    preprocess input_file preprocessed_file;
    let source = read_file preprocessed_file in
    remove_if_exists preprocessed_file;

    match stage with
    | Lex ->
        let tokens = Lexer.lex source in
        List.iter (fun t -> print_endline (Lexer.show_token t)) tokens
    | Parse ->
        let tokens = Lexer.lex source in
        let ast = Parser.parse tokens in
        print_endline (Ast.pp_program ast)
    | Validate ->
        let tokens = Lexer.lex source in
        let ast = Parser.parse tokens in
        let resolved_ast = Semanticanalysis.resolve_program ast in
        let _ = Semanticanalysis.typecheck_program resolved_ast in
        ()
    | Tackygen ->
        let tokens = Lexer.lex source in
        let ast = Parser.parse tokens in
        let resolved_ast = Semanticanalysis.resolve_program ast in
        let validated_ast = Semanticanalysis.typecheck_program resolved_ast in
        let tacky = Tackygen.gen_program validated_ast in
        print_endline (Tacky.pp_program tacky)
    | Codegen ->
        let tokens = Lexer.lex source in
        let ast = Parser.parse tokens in
        let resolved_ast = Semanticanalysis.resolve_program ast in
        let validated_ast = Semanticanalysis.typecheck_program resolved_ast in
        let tacky = Tackygen.gen_program validated_ast in
        let _ = Codegen.gen_program tacky in
        ()
    | Asm ->
        let tokens = Lexer.lex source in
        let ast = Parser.parse tokens in
        let resolved_ast = Semanticanalysis.resolve_program ast in
        let validated_ast = Semanticanalysis.typecheck_program resolved_ast in
        let tacky = Tackygen.gen_program validated_ast in
        let asm_ast = Codegen.gen_program tacky in
        let asm_text = Emit.emit_program asm_ast in
        write_file asm_file asm_text
    | Object ->
        let tokens = Lexer.lex source in
        let ast = Parser.parse tokens in
        let resolved_ast = Semanticanalysis.resolve_program ast in
        let validated_ast = Semanticanalysis.typecheck_program resolved_ast in
        let tacky = Tackygen.gen_program validated_ast in
        let asm_ast = Codegen.gen_program tacky in
        let asm_text = Emit.emit_program asm_ast in
        write_file asm_file asm_text;
        assemble asm_file output_file
    | Full ->
        let tokens = Lexer.lex source in
        let ast = Parser.parse tokens in
        let resolved_ast = Semanticanalysis.resolve_program ast in
        let validated_ast = Semanticanalysis.typecheck_program resolved_ast in
        let tacky = Tackygen.gen_program validated_ast in
        let asm_ast = Codegen.gen_program tacky in
        let asm_text = Emit.emit_program asm_ast in
        write_file asm_file asm_text;
        assemble_and_link asm_file output_file
  with
  | Lexer.LexError msg -> fail ("Lexing error: " ^ msg)
  | Parser.ParseError msg -> fail ("Parsing error: " ^ msg)
  | Semanticanalysis.SemanticError msg -> fail ("Semantic error: " ^ msg)
  | e -> fail ("Internal error: " ^ Printexc.to_string e)
