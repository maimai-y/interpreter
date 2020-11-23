open Syntax
open Value

(* 実際の計算をする関数 *)
(* Eval.f : Syntax.t Env.t -> Value.t *)
let rec f expr env = match expr with
    Number (n) -> VNumber (n)
  | Bool (b) -> VBool (b)
  | Var (var) ->
      begin try Env.get env var with
        Env.Not_found -> failwith ("Unbound variable: " ^ var)
      end
  | Nil -> VList([])
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
      let value = f arg2 env in
        f arg3 (Env.extend env arg1 value)

  | Letrec (arg1, arg2, arg3, arg4) ->
      f arg4 (Env.extend env arg1 (VCloR (arg1, arg2, arg3, env)))

  | Fun (arg1, arg2) ->
      VClo (arg1, arg2, env)

  | App (arg1, arg2) ->
      let v1 = f arg1 env in
      begin match v1 with
          VClo (x, t, env_fun) ->
            let v2 = f arg2 env in
              f t (Env.extend env_fun x v2)
        | VCloR (g, x, t, env_fun) ->
            let v2 = f arg2 env in
            let new_env1 = Env.extend env_fun x v2 in
            let new_env2 = Env.extend new_env1 g v1 in
              f t new_env2
        | _ -> failwith ("Not a function: "
                        ^ Syntax.to_string arg1)
      end
  | Cons (arg1, arg2) ->
      let v1 = f arg1 env in
      let v2 = f arg2 env in
      begin match v2 with
          VList (l) -> VList (v1::l)
        | _ -> failwith ("Not a list: "
                          ^ Syntax.to_string arg1)
      end
  | Match (arg1, arg2, arg3, arg4, arg5) ->
      begin match f arg1 env with
          VList (l) ->
            begin match l with
                [] -> f arg2 env
              | first :: rest ->
                  let new_env1 = Env.extend env arg3 first in
                  let new_env2 = Env.extend new_env1 arg4 (VList (rest)) in
                  f arg5 new_env2
            end
        | _ -> failwith ("Not a list: "
                        ^ Syntax.to_string arg1)
      end
