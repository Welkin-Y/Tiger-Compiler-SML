
(* dummy Translate *)
structure Translate = struct type exp = unit end

structure Semant :> SEMANT =
struct
    type tyvenv = Env.enventry Symbol.table
    type tytenv = T.ty Symbol.table
    type expty = {exp: Translate.exp, ty: T.ty}
    

    fun transVar (venv , tenv, var) = {exp = (), ty = T.INT}
    and transExp (venv : tyvenv, tenv: tytenv, exp: A.exp) = 
        case exp of 
            (*To check an IfExp we need to make sure: 1. test is int 2. then and else have same types*)
            A.IfExp {test, then', else', pos} => 
            let 
                val {exp=expelse, ty=tyelse} = case else' of
                    NONE => {exp=(), ty=T.NIL}
                    | SOME e => transExp (venv, tenv, e)
                val {exp=_, ty=tythen} = transExp (venv, tenv, then')
                val {exp=_, ty=tytest} = transExp (venv, tenv, test) 
            in
                case tytest of
                    T.INT => ()
                    | _ => raise ErrorMsg.Error;
                if tythen <> tyelse then ErrorMsg.error pos (" Unmatched type: then exp is " ^ T.toString tythen ^ ", else exp is " ^ T.toString tyelse) else ();
                {exp=(), ty=tythen}
            end
            | A.OpExp {left, oper, right, pos} =>
            let
                val {exp=_, ty=tyleft} = transExp (venv, tenv, left)
                val {exp=_, ty=tyright} = transExp (venv, tenv, right)
            in
                case oper of
                    A.PlusOp => if tyleft = T.INT andalso tyright = T.INT then {exp=(), ty=T.INT}
                                else raise ErrorMsg.Error
                    | A.MinusOp => if tyleft = T.INT andalso tyright = T.INT then {exp=(), ty=T.INT}
                                else raise ErrorMsg.Error
                    | A.TimesOp => if tyleft = T.INT andalso tyright = T.INT then {exp=(), ty=T.INT}
                                else raise ErrorMsg.Error
                    | A.DivideOp => if tyleft = T.INT andalso tyright = T.INT then {exp=(), ty=T.INT}
                                else raise ErrorMsg.Error
                    | A.EqOp => if tyleft = tyright then {exp=(), ty=T.INT}
                                else raise ErrorMsg.Error
                    | A.NeqOp => if tyleft = tyright then {exp=(), ty=T.INT}
                                else raise ErrorMsg.Error
                    | A.LtOp => if tyleft = tyright then {exp=(), ty=T.INT}
                                else raise ErrorMsg.Error
                    | A.LeOp => if tyleft = tyright then {exp=(), ty=T.INT}
                                else raise ErrorMsg.Error
                    | A.GtOp => if tyleft = tyright then {exp=(), ty=T.INT}
                                else raise ErrorMsg.Error
                    | A.GeOp => if tyleft = tyright then {exp=(), ty=T.INT}
                                else raise ErrorMsg.Error
            end
            (* Check Let Exp: 1. gothrough decs 2. go through body*)
            | A.LetExp {decs, body, pos} => let
                val {venv=venv', tenv=tenv'} = foldl (fn (dec, {venv, tenv}) => transDec (venv, tenv, dec)) {venv=venv, tenv=tenv} decs
            in
                print ("begin to check body\n");
                transExp (venv', tenv', body)
            end
            | A.IntExp _ => {exp=(), ty=T.INT}
            | A.StringExp _ => {exp=(), ty=T.STRING}
            | A.NilExp => {exp=(), ty=T.NIL}
            | A.SeqExp exps => let
                val explist : Translate.exp list = []
                fun helper ((exp, _), ty) = let
                    val {exp=entryExp, ty=entryTy} = transExp (venv, tenv, exp)
                in
                    explist = explist @ [entryExp];
                    entryTy
                end
            in
                {exp=(), ty=foldl helper T.NIL exps}
            end
            

    and transDec (venv : tyvenv, tenv: tytenv, dec : A.dec) = 
        case dec of
            (*To check a VarDec: 0. TODO: the proposed type exists 1. the type of init should be same as ty if there are ty 2. add Var to venv*)
            A.VarDec{name, escape, typ, init, pos}

             => let
                val {exp=_, ty=tyinit} = transExp (venv, tenv, init)
                val newtyp = case typ of
                    NONE => tyinit
                    | SOME t => case Symbol.look (tenv, #1 t) of 
                        NONE => raise ErrorMsg.Error
                        | SOME ty => ty
                in
                if newtyp = tyinit then 
                    let 
                    val newVenv =  Symbol.enter(venv, name, Env.VarEntry {ty=tyinit})
                    val _ = PrintEnv.printEnv (newVenv,tenv)
                    in
                    {venv = newVenv, tenv = tenv}
                    end
                    else raise ErrorMsg.Error
                end
            | A.TypeDec tydecs => 
            let 
            val {venv=newvenv, tenv=newtenv} = foldl (fn (tydec, {venv, tenv}) => transTyDec (venv, tenv, tydec)) {venv=venv, tenv=tenv} tydecs
            in
                PrintEnv.printEnv (newvenv, newtenv);
                {venv=newvenv, tenv=newtenv}
            end
    and transTyDec (venv, tenv, tydec) = 
        let
            val {name=n, ty=t, pos=p} = tydec
        in 
        case t of
            (* TODO: cross recursion check e.g. type a = b; type b = a & mutual recursive type def & self recursive type def*)
            A.NameTy (nRefTo, p') => 
            let
                val t = case Symbol.look (tenv, nRefTo) of
                    NONE => raise ErrorMsg.Error
                    | SOME ty => SOME ty
            in
                {venv=venv, tenv=Symbol.enter (tenv, n, T.NAME (n, (ref t)))}
            end
           (*TODO: ArrayTy & RecordTy*)
           | A.ArrayTy (nRefTo, p') =>
            let
                val t = case Symbol.look (tenv, nRefTo) of
                    NONE => raise ErrorMsg.Error
                    | SOME ty => ty
            in
                {venv=venv, tenv=Symbol.enter (tenv, n, T.ARRAY (t, ref ()))}
            end
            | A.RecordTy fields =>
            let
                val fieldlist = []
                fun helper (field, fieldlist) = let
                    val {name, escape, typ, pos} = field
                    val ty = case Symbol.look (tenv, typ) of
                        NONE => raise ErrorMsg.Error
                        | SOME ty => ty
                in
                    (name, ty)::fieldlist

                end
                val newfieldlist = foldr helper fieldlist fields
                val recordTy = T.RECORD (newfieldlist, ref())
            in
                {venv=venv, tenv=Symbol.enter (tenv, n, recordTy)}
            end
        end
    and transTy (tenv, ty) = T.INT
    and transProg (exp: A.exp) = (
        let 
            val venv = Env.base_venv
            val tenv = Env.base_tenv 
        in

            transExp (venv, tenv, exp);
            PrintEnv.printEnv (venv, tenv)
        end;
        print "\nSemantic Analysis Succeed\n"
    )
end