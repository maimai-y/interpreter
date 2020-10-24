open Syntax
open Value

(* 実際の計算をする関数 *)
(* Eval.f : Syntax.t -> Value.t *)
let rec f expr env = match expr with
    Number (n) -> VNumber (n)
  | Bool (b) -> VBool (b)
  | Var (var) ->
      begin try Env.get env var with
        Env.Not_found -> failwith ("Unbound variable: " ^ var)
      end
  | Op (arg1, Plus, arg2) ->
      let v1 = f arg1 env in
      let v2 = f arg2 env in
      begin match (v1, v2) with
          (VNumber (n1), VNumber (n2)) -> VNumber (n1 + n2)
        | (_, _) -> failwith ("Bad arguments to +: " ^
                              Value.to_string v1 ^ ", " ^
                              Value.to_string v2)
      end
  | Op (arg1, Minus, arg2) ->
      let v1 = f arg1 env in
      let v2 = f arg2 env in
      begin match (v1, v2) with
          (VNumber (n1), VNumber (n2)) -> VNumber (n1 - n2)
        | (_, _) -> failwith ("Bad arguments to -: " ^
                              Value.to_string v1 ^ ", " ^
                              Value.to_string v2)
      end
  | Op (arg1, Times, arg2) ->
      let v1 = f arg1 env in
      let v2 = f arg2 env in
      begin match (v1, v2) with
          (VNumber (n1), VNumber (n2)) -> VNumber (n1 * n2)
        | (_, _) -> failwith ("Bad arguments to *: " ^
                              Value.to_string v1 ^ ", " ^
                              Value.to_string v2)
      end
  | Op (arg1, Less, arg2) ->
      let v1 = f arg1 env in
      let v2 = f arg2 env in
      begin match (v1, v2) with
          (VNumber (n1), VNumber (n2)) -> 
              if n1 < n2 then VBool (true)
              else VBool (false)
        | (_, _) -> failwith ("Bad arguments to <: " ^
                              Value.to_string v1 ^ ", " ^
                              Value.to_string v2)
      end
  | Op (arg1, Equal, arg2) ->
      let v1 = f arg1 env in
      let v2 = f arg2 env in
      begin match (v1, v2) with
          (VNumber (n1), VNumber (n2)) -> 
              if n1 = n2 then VBool (true)
              else VBool (false)
        | (_, _) -> failwith ("Bad arguments to =: " ^
                              Value.to_string v1 ^ ", " ^
                              Value.to_string v2)
      end
  | If (arg1, arg2, arg3) ->
      let v1 = f arg1 env in
      begin match v1 with
          VBool (true) -> f arg2 env
        | VBool (false) -> f arg3 env
        | _ -> failwith ("Predicate is not a boolean: " ^
                         Value.to_string v1)
      end
  | Let (arg1, arg2, arg3) ->
      begin match arg1 with
          Var (var) ->
            let value = f arg2 env in
              f arg3 (Env.extend env var value)
        | _ -> failwith (Syntax.to_string arg1 
                        ^ " is not an appropriate name for a variable.")
      end
