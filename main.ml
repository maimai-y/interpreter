(* メイン関数 *)
let go () =
  let program = Parser.start Lexer.token (Lexing.from_channel stdin) in
  (* これで標準入力を字句解析して、構文解析した結果を program に入れ *)
  if Array.length Sys.argv = 1 then begin
    (* 引数がないなら（-no-message が指定されていないなら）*)
    print_string "Parsed : ";
    Syntax.print program;               (* 入力を表示する *)
    print_newline ();
    print_string "Result : "
  end;
  let str =
    match Eval.f program Env.empty (fun v -> Eval.Ok (v)) with
        Eval.Ok (v) -> Value.to_string v
      | Eval.Err (n) -> "Error " ^ Value.to_string n in
  print_string (str);      (* 結果を表示する *)
  print_newline ()

(* スタートアップ *)
let _ = go ()
