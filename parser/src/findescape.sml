structure A = Absyn
structure S = Symbol
structure FindEscape: sig val findEscape: A.exp -> unit end =
struct
	type depth = int
	type escEnv = (depth * bool ref) S.table
	
	fun traverseVar(env: escEnv, d: depth, s: A.var): unit =
			case s of A.SimpleVar(name, _) => 
					(case S.look(env, name) of SOME (d', r) => if d > d' then r := true else ()
						| NONE => ())
			| A.FieldVar(var, _, _) => traverseVar(env, d, var)
			| A.SubscriptVar(var, exp, _) => (traverseExp(env, d, exp);traverseVar(env, d, var))

	and traverseExp(env:escEnv, d:depth, s:A.exp): unit = 
			case s of A.VarExp(var) => traverseVar(env, d, var)
			| A.NilExp => ()
			| A.IntExp(_) => ()
			| A.StringExp(_, _) => ()
			| A.CallExp({func, args, pos}) => (List.app (fn arg => traverseExp(env, d, arg)) args) 
			| A.OpExp({left, oper, right, pos}) => (traverseExp(env, d, left); traverseExp(env, d, right))
			(*fields = (symbol * exp * pos) list *)
			| A.RecordExp({fields, typ, pos}) => (List.app (fn field => traverseExp(env, d, #2 field)) fields)
			(*expseq = (exp * pos) list *)
			| A.SeqExp(expseq) => (List.app (fn exp => traverseExp(env, d, #1 exp)) expseq)
			| A.AssignExp({var, exp, pos}) => (traverseVar(env, d, var); traverseExp(env, d, exp))
			| A.IfExp({test, then', else', pos}) => (
					traverseExp(env, d, test); traverseExp(env, d, then'); 
					case else' of SOME exp => traverseExp(env, d, exp) 
					| NONE => ())
			| A.WhileExp({test, body, pos}) => (traverseExp(env, d, test); traverseExp(env, d, body))
			| A.ForExp({var, escape, lo, hi, body, pos}) => (
					traverseExp(env, d, lo); 
					traverseExp(env, d, hi);
					escape := false; (* initially not escaping *)
					let val newEnv = S.enter(env, var, (d, escape)) in traverseExp(newEnv, d, body) end 
				) 
			| A.BreakExp(_) => ()
			| A.LetExp({decs, body, pos}) => (
					let val newEnv = traverseDecs(env, d + 1, decs) in 
						traverseExp(newEnv, d + 1, body) end
				)
			| A.ArrayExp({typ, size, init, pos}) => (traverseExp(env, d, size); traverseExp(env, d, init))

		(* Whenever a variable or formal-parameter declaration is found at static function-nesting depth d then the bool ref r is assigned false ; the binding a -> (d, r) is entered into the env. *)
	and traverseDecs(env:escEnv, d:depth, s: Absyn.dec list): escEnv =
			case s of nil => env
				(* var: {name: symbol, escape: bool ref, typ: (symbol * pos) option, init: exp, pos: pos} *)
			| A.VarDec(var)::l => (
					traverseExp(env, d, #init var);
					#escape var := false;
					traverseDecs(S.enter(env, #name var, (d, #escape var)), d, l)
				)
			(* funcs {name: symbol, params: field list, result: (symbol * pos) option, body: exp, pos: pos} list *)
			| A.FunctionDec(funcs)::l => (
					let
						fun traversefunc(func: A.fundec) =
								let	fun traversparam(param: A.field, env: escEnv) =
											(
												#escape param := false; 
												S.enter(env, #name param, (d+1, #escape param))
											)
									val newEnv = foldl traversparam env (#params func)
								in
									traverseExp(newEnv, d + 1, #body func)
								end
					in
						List.app traversefunc funcs;
						traverseDecs(env, d, l)
					end

				)
			| A.TypeDec(types)::l => traverseDecs(env, d, l)

	fun findEscape(prog: Absyn.exp): unit =
			traverseExp(Symbol.empty, 0, prog) 
end
