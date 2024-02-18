
(* dummy Translate *)
structure Translate = struct type exp = unit end

structure Semant :> SEMANT =
struct
    type tyvenv = Env.enventry Symbol.table
    type tytenv = T.ty Symbol.table
    type expty = {exp: Translate.exp, ty: T.ty}

    fun transVar (venv , tenv, var) = {exp = (), ty = T.INT}
    and transExp (venv : tyvenv ref, tenv: tytenv ref, exp: A.exp) = 
        case exp of 
            (*To check an IfExp we need to make sure: 1. test is int 2. then and else have same types*)
            A.IfExp {test, then', else', pos} => let 
                                                val {exp=expelse, ty=tyelse} = case else' of
                                                    NONE => {exp=(), ty=T.INT}
                                                    | SOME e => transExp (venv, tenv, e)
                                                val {exp=_, ty=tythen} = transExp (venv, tenv, then')
                                                val {exp=_, ty=tytest} = transExp (venv, tenv, test) 
                                                in
                                                case tytest of
                                                    T.INT => ()
                                                    | _ => raise ErrorMsg.Error;
                                                if expelse = () then {exp=(), ty=tythen}
                                                else if tythen = tyelse then {exp=(), ty=tythen}
                                                else raise ErrorMsg.Error
                                                end
            (* Check Let Exp: 1. gothrough decs 2. go through body*)
            | A.LetExp {decs, body, pos} => let
                                            val {venv=venv', tenv=tenv'} = foldl (fn (dec, {venv, tenv}) => transDec (venv, tenv, dec)) {venv=venv, tenv=tenv} decs
                                            in
                                            transExp (venv', tenv', body)
                                            end
            | A.IntExp _ => {exp=(), ty=T.INT}
            | A.StringExp _ => {exp=(), ty=T.STRING}
            | A.NilExp => {exp=(), ty=T.NIL}
            (*Seq Exp do nothing for now*)
            | A.SeqExp exps => {exp=(), ty=T.UNIT}

    and transDec (venv : tyvenv ref, tenv: tytenv ref, dec : A.dec) = 
        case dec of
            (*To check a VarDec: 1. the type of init should be same as ty if there are ty 2. add Var to venv*)
            A.VarDec{name, escape, typ, init, pos}

             => let
                val {exp=_, ty=tyinit} = transExp (venv, tenv, init)
                val newtyp = case typ of
                    NONE => tyinit
                    | SOME t => case Symbol.look (!(tenv), #1 t) of 
                        NONE => raise ErrorMsg.Error
                        | SOME typppp => typppp
                in
                if newtyp = tyinit then {venv = let val newEnv: tyvenv = Symbol.enter(!(venv), name, Env.VarEntry {ty=tyinit}) in ref(newEnv) end, tenv = tenv}
                    else raise ErrorMsg.Error
                end
          
    and transTy (tenv, ty) = T.INT
    and transProg (exp: A.exp) = (
        let 
            val venv = let val env: tyvenv = Env.base_venv in ref env end
            val tenv = let val env: tytenv = Env.base_tenv in ref env end
        in
            transExp (venv, tenv, exp)
        end;
        print "\nSemantic Analysis Succeed\n"
    )
    and prTransProg (exp: A.exp) = (
        let 
            val venv = let val env: tyvenv = Env.base_venv in ref env end
            val tenv = let val env: tytenv = Env.base_tenv in ref env end
        in
            transExp (venv, tenv, exp);
            PrintEnv.printTyEnv (!tenv);
            PrintEnv.printVarEnv (!venv)
        end;
        print "\nSemantic Analysis Succeed\n"
    )
end