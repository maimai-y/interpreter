open Syntax

(* 型を同じにできない場合に起きる例外 *)
exception Unify of Type.t * Type.t

(* r が型 ty に現れるかをチェックする (occur check) *)
(* occur : Type.t option ref -> Type.t -> bool *)
let rec occur r ty = match ty with
    Type.TInt -> false
  | Type.TBool -> false
  | Type.TFun (ty1, ty2) -> occur r ty1 || occur r ty2
  | Type.TVar(r') ->
      if r == r' then true
      else begin match !r' with
          None -> false
        | Some (ty') -> occur r ty'
      end
  | Type.Tlist(ty') -> occur r ty'

(* ty1 = ty2 となるように、型変数への代入をする *)
(* unify : Type.t -> Type.t -> unit *)
let rec unify ty1 ty2 = match (ty1, ty2) with
    (Type.TInt, Type.TInt) -> ()
  | (Type.TBool, Type.TBool) -> ()
  | (Type.TFun (ty1, ty1'), Type.TFun (ty2, ty2')) ->
      begin
        unify ty1 ty2;
        unify ty1' ty2'
      end
  | (Type.Tlist (ty1), Type.Tlist (ty2)) ->
      unify ty1 ty2
  (* この下の2行は必要。例えば
      TVar(r1←中身None),TVar(r2)だった時、
      ty1がTVar(r1←中身Some(TVar(r2)))になってしまい、例えば
      TVar(r2←中身None),TVar(r1←中身Some(TVar(r2)))が来た時、raiseする
      例：let rec f x = f (x - 1) in f 1 + 2 *)
  | (Type.TVar ({ contents = Some(ty1') }), _) -> unify ty1' ty2
  | (_, Type.TVar ({ contents = Some(ty2') })) -> unify ty1 ty2'
  | (Type.TVar (r1), Type.TVar (r2)) when r1 == r2 -> ()
  | (Type.TVar (r1), _) ->
      begin match !r1 with
          None -> if occur r1 ty2 then raise (Unify (ty1, ty2))
                                  else r1 := Some (ty2)
        | Some (ty1') -> unify ty1' ty2
      end
  | (_, Type.TVar (r2)) ->
      begin match !r2 with
          None -> if occur r2 ty1 then raise (Unify (ty1, ty2))
                                  else r2 := Some (ty1)
        | Some (ty2') -> unify ty1 ty2'
      end
  | (_, _) -> raise (Unify (ty1, ty2))

(* 型推論 *)
(* g : Syntax.t -> (string, Type.t) Env.t -> Type.t *)
let rec g expr tenv =
  try
    begin match expr with
        Number (num) -> Type.TInt
      | Bool (b) -> Type.TBool
      | Var (var) ->
          begin try Env.get tenv var with
            Env.Not_found -> failwith ("Unbound variable: " ^ var)
          end
      | Op (t1, op, t2) ->
          begin match op with
              Plus | Minus | Times | Div ->
                let ty1 = g t1 tenv in
                let ty2 = g t2 tenv in
                unify ty1 Type.TInt;
                unify ty2 Type.TInt;
                Type.TInt
            | Less | Equal ->
                let ty1 = g t1 tenv in
                let ty2 = g t2 tenv in
                unify ty1 Type.TInt;
                unify ty2 Type.TInt;
                Type.TBool
          end
      | If (t1, t2, t3) ->
          let ty1 = g t1 tenv in
          let ty2 = g t2 tenv in
          let ty3 = g t3 tenv in
          unify ty1 Type.TBool;
          unify ty2 ty3;
          ty2
      | Let (x, t1, t2) ->
          let ty1 = g t1 tenv in
          let tenv' = Env.extend tenv x ty1 in
          g t2 tenv'
      | Fun (x, t) ->
          let ty1 = Type.gen_type () in
          let tenv' = Env.extend tenv x ty1 in
          let ty2 = g t tenv' in
          Type.TFun (ty1, ty2)
      | Letrec (f, x, t1, t2) ->
          let tyx = Type.gen_type () in
          let ty1 = Type.gen_type () in
          let tenv' = Env.extend tenv f (Type.TFun (tyx, ty1)) in
          let tenv'' = Env.extend tenv' x tyx in
          unify (g t1 tenv'') ty1;
          g t2 tenv'
      (* unifyでなくfailwithを使って書きそうになったけど
         fun x -> fun n -> x nの時を考えるとunifyだと思った *)
      | App (t1, t2) ->
          let tyx = Type.gen_type () in
          let tyv = Type.gen_type () in
          unify (g t1 tenv) (Type.TFun (tyx, tyv));
          unify (g t2 tenv) tyx;
          tyv
      | Nil ->
          let ty = Type.gen_type() in Type.Tlist (ty)
      | Cons (t1, t2) ->
          let ty = g t1 tenv in
          unify (g t2 tenv) (Type.Tlist (ty));
          Type.Tlist (ty)
      | Match (t1, t2, x1, x2, t3) ->
          let ty1 = Type.gen_type() in
          unify (g t1 tenv) (Type.Tlist (ty1));
          let ty = g t2 tenv in
          let tenv' = Env.extend tenv x1 ty1 in
          let tenv'' = Env.extend tenv' x2 (Type.Tlist (ty1)) in
          unify ty (g t3 tenv'');
          ty
      | Raise (t) ->
          unify (g t tenv) Type.TInt;
          let ty = Type.gen_type() in
          ty
      | Try (t1, x, t2) ->
          let ty = g t1 tenv in
          let tenv' = Env.extend tenv x Type.TInt in
          let ty2 = g t2 tenv' in
          unify ty ty2;
          ty
    end
  with Unify (ty1, ty2) -> begin (* unify できなかった *)
    print_endline "式";
    print_string "	";
    Syntax.print expr;
    print_newline ();
    print_endline "を型推論中に型エラーがおきました。";
    print_string "	";
    Type.print ty1;
    print_newline ();
    print_endline "と";
    print_string "	";
    Type.print ty2;
    print_newline ();
    print_endline "は unify できません。";
    exit 0
  end

(* 型推論の入り口 *)
(* Typing.f : Syntax.t -> Type.t *)
let f expr =
  let ty = g expr Env.empty in
  ty
