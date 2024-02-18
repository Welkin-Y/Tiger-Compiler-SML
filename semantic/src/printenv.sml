(* Print env *)
structure PrintEnv =
struct
    type tyvenv = Env.enventry Symbol.table
    type tytenv = T.ty Symbol.table

    (* I think this fun should move to Types*)
    fun toString ty = case ty of 
            T.INT =>  "int\n"
          | T.STRING => "string\n"
          | T.NIL =>  "nil\n"
          | T.UNIT => "unit\n"
          | T.RECORD (tylist, _) => "record\n" (* TODO *)
          | T.ARRAY (ty, _) =>  "array of" ^ (toString ty)  
          | T.NAME (symbol, tyref) => "name" ^ Symbol.name(symbol) ^ "\n" (* TODO *)

    fun printTyEnv(env: tytenv) = (
        print "Type Environment:\n";
        (* Print each types *)
        let fun helper ty = print(toString ty)  
        in Symbol.app helper env end
    )

    fun printVarEnv(env: tyvenv) = (
        print "Var Environment:\n";
        let fun helper entry = case entry of
                Env.VarEntry {ty: T.ty} => print("var: " ^ (toString ty) ^ "\n")
              | Env.FunEntry {formals: T.ty list, result: T.ty} => print("fun: " ^ (toString result) ^ "\n")
        in Symbol.app helper env end
    )
end
