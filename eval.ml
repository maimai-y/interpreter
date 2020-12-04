open Syntax
open Value

(* 実際の計算をする関数 *)
(* Eval.f : Syntax.t -> (string, Value.t) Env.t -> (Value.t -> 'a) -> 'a *)
let rec f expr env cont = match expr with
    Number (n) -> cont (VNumber (n))
  | Bool (b) -> cont (VBool (b))
  | Var (var) ->
      begin try cont (Env.get env var) with
        Env.Not_found -> failwith ("Unbound variable: " ^ var)
      end
  | Nil -> cont (VList ([]))
  | Op (arg1, Plus, arg2) ->
      f arg1 env (fun v1 -> f arg2 env (fun v2 ->
        begin match (v1, v2) with
            (VNumber (n1), VNumber (n2)) -> cont (VNumber (n1 + n2))
          | (_, _) -> failwith ("Bad arguments to +: " ^
                                Value.to_string v1 ^ ", " ^
                                Value.to_string v2)
        end))
  | Op (arg1, Minus, arg2) ->
      f arg1 env (fun v1 -> f arg2 env (fun v2 ->
        begin match (v1, v2) with
            (VNumber (n1), VNumber (n2)) -> cont (VNumber (n1 - n2))
          | (_, _) -> failwith ("Bad arguments to -: " ^
                                Value.to_string v1 ^ ", " ^
                                Value.to_string v2)
        end))
  | Op (arg1, Times, arg2) ->
      f arg1 env (fun v1 -> f arg2 env (fun v2 ->
        begin match (v1, v2) with
            (VNumber (n1), VNumber (n2)) -> cont (VNumber (n1 * n2))
          | (_, _) -> failwith ("Bad arguments to *: " ^
                                Value.to_string v1 ^ ", " ^
                                Value.to_string v2)
        end))
  | Op (arg1, Less, arg2) ->
      f arg1 env (fun v1 -> f arg2 env (fun v2 ->
        begin match (v1, v2) with
            (VNumber (n1), VNumber (n2)) -> 
                if n1 < n2 then cont (VBool (true))
                else cont(VBool (false))
          | (_, _) -> failwith ("Bad arguments to <: " ^
                                Value.to_string v1 ^ ", " ^
                                Value.to_string v2)
        end))
  | Op (arg1, Equal, arg2) ->
      f arg1 env (fun v1 -> f arg2 env (fun v2 ->
        begin match (v1, v2) with
            (VNumber (n1), VNumber (n2)) -> 
                if n1 = n2 then cont (VBool (true))
                else cont (VBool (false))
          | (_, _) -> failwith ("Bad arguments to =: " ^
                                Value.to_string v1 ^ ", " ^
                                Value.to_string v2)
        end))
  | If (arg1, arg2, arg3) ->
      f arg1 env (fun v1 ->
        begin match v1 with
            VBool (true) -> f arg2 env (fun x -> cont x)
          | VBool (false) -> f arg3 env (fun x -> cont x)
          | _ -> failwith ("Predicate is not a boolean: " ^
                          Value.to_string v1)
        end)
  | Let (arg1, arg2, arg3) ->
      f arg2 env (fun value ->
          f arg3 (Env.extend env arg1 value) (fun x -> cont x))

  | Letrec (arg1, arg2, arg3, arg4) ->
      f arg4 (Env.extend env arg1 (VCloR (arg1, arg2, arg3, env))) cont

  | Fun (arg1, arg2) ->
      cont (VClo (arg1, arg2, env))

  | App (arg1, arg2) ->
      f arg1 env (fun v1 ->
        begin match v1 with
            VClo (x, t, env_fun) ->
              f arg2 env (fun v2 ->
                f t (Env.extend env_fun x v2) cont)
          | VCloR (g, x, t, env_fun) ->
              f arg2 env (fun v2 ->
              let new_env1 = Env.extend env_fun x v2 in
              let new_env2 = Env.extend new_env1 g v1 in
                f t new_env2 cont)
          | _ -> failwith ("Not a function: "
                          ^ Syntax.to_string arg1)
        end)
  | Cons (arg1, arg2) ->
      f arg1 env (fun v1 ->
        f arg2 env (fun v2 ->
        begin match v2 with
            VList (l) -> cont (VList (v1::l))
          | _ -> failwith ("Not a list: "
                            ^ Syntax.to_string arg1)
        end))
  | Match (arg1, arg2, arg3, arg4, arg5) ->
      f arg1 env (fun v1 ->
        begin match v1 with
            VList (l) ->
              begin match l with
                  [] -> f arg2 env (fun x -> cont x) 
                | first :: rest ->
                    let new_env1 = Env.extend env arg3 first in
                    let new_env2 = Env.extend new_env1 arg4 (VList (rest)) in
                    f arg5 new_env2 (fun x -> cont x)
              end
          | _ -> failwith ("Not a list: "
                          ^ Syntax.to_string arg1)
        end)