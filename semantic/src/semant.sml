
(* dummy Translate *)
structure Translate = struct type exp = unit end
structure T = Types
structure Semant :> SEMANT =
struct
    type venv = Env.enventry Symbol.table
    type tenv = T.ty Symbol.table
    type expty = {exp: Translate.exp, ty: T.ty}


    fun transVar (venv, tenv, var) = {exp = (), ty = T.INT}
    and transExp (venv, tenv, exp) = 
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
            | A.IntExp _ => {exp=(), ty=T.INT}
            | A.StringExp _ => {exp=(), ty=T.STRING}
            | A.NilExp => {exp=(), ty=T.NIL}

    and transDec (venv, tenv, dec) = {venv=venv, tenv=tenv}
    and transTy (tenv, ty) = T.INT
    fun transProg (exp: A.exp) = (transExp (Env.base_venv, Env.base_tenv, exp); print "\nSemantic Analysis Succeed\n";())
end