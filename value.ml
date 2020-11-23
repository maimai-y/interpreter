(* Value.t : プログラムの実行結果を表す型 *)
type t = VNumber of int
       | VBool of bool
       | VClo of string * Syntax.t * (string, t) Env.t
       | VCloR of string * string * Syntax.t * (string, t) Env.t
       | VList of t list

(* プログラムの実行結果を文字列にする関数 *)
(* Value.to_string : Value.t -> string *)
let rec to_string value = match value with
    VNumber (n) -> string_of_int n
  | VBool (b) -> if b then "true" else "false"
  | VClo (x, t, env) -> "<fun>"
  | VCloR (g, x, t, env) -> "<rec fun>"
  | VList (l) ->
      match l with
          [] -> "[]" 
        | first :: rest -> to_string first ^ " :: " ^ to_string (VList (rest))

(* プログラムの実行結果をプリントする関数 *)
(* Value.print : Value.t -> unit *)
let print exp =
  let str = to_string exp in
  print_string str
