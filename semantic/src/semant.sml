
(* dummy Translate *)
structure Translate = struct type exp = unit end

structure Semant :> SEMANT =
struct
    type tyvenv = Env.enventry Symbol.table
    type tytenv = T.ty Symbol.table
    type expty = {exp: Translate.exp, ty: T.ty}
    
    fun transExp (venv : tyvenv, tenv: tytenv, exp: A.exp) = 
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
                        if not (T.equals(tythen, tyelse)) then ErrorMsg.error pos (" Unmatched type: then exp is " ^ T.toString tythen ^ ", else exp is " ^ T.toString tyelse) else ();
                        {exp=(), ty=tythen}
                    end
            | A.OpExp {left, oper, right, pos} =>
                let
                    val {exp=_, ty=tyleft} = transExp (venv, tenv, left)
                    val {exp=_, ty=tyright} = transExp (venv, tenv, right)
                in
                    case oper of
                        A.PlusOp => if T.equals(tyleft, T.INT) andalso T.equals(tyright, T.INT) then {exp=(), ty=T.INT}
                            else raise ErrorMsg.Error
                    | A.MinusOp => if T.equals(tyleft, T.INT) andalso T.equals(tyright, T.INT) then {exp=(), ty=T.INT}
                        else raise ErrorMsg.Error
                    | A.TimesOp => if T.equals(tyleft, T.INT) andalso T.equals(tyright, T.INT) then {exp=(), ty=T.INT}
                        else raise ErrorMsg.Error
                    | A.DivideOp => if T.equals(tyleft, T.INT) andalso T.equals(tyright, T.INT) then {exp=(), ty=T.INT}
                        else raise ErrorMsg.Error
                    | A.EqOp => if T.equals(tyleft, tyright) then {exp=(), ty=T.INT}
                        else raise ErrorMsg.Error
                    | A.NeqOp => if T.equals(tyleft, tyright) then {exp=(), ty=T.INT}
                        else raise ErrorMsg.Error
                    | A.LtOp => if T.equals(tyleft, tyright) then {exp=(), ty=T.INT}
                        else raise ErrorMsg.Error
                    | A.LeOp => if T.equals(tyleft, tyright) then {exp=(), ty=T.INT}
                        else raise ErrorMsg.Error
                    | A.GtOp => if T.equals(tyleft, tyright) then {exp=(), ty=T.INT}
                        else raise ErrorMsg.Error
                    | A.GeOp => if T.equals(tyleft, tyright) then {exp=(), ty=T.INT}
                        else raise ErrorMsg.Error
                end
            (* Check Let Exp: 1. gothrough decs 2. go through body*)
            | A.LetExp {decs, body, pos} => 
                let
                    val {venv=venv', tenv=tenv'} = foldl (fn (dec, {venv, tenv}) => transDec (venv, tenv, dec)) {venv=venv, tenv=tenv} decs
                in
                    print ("begin to check body\n");
                    transExp (venv', tenv', body)
                end
            | A.IntExp _ => {exp=(), ty=T.INT}
            | A.StringExp _ => {exp=(), ty=T.STRING}
            | A.NilExp => {exp=(), ty=T.NIL}
            | A.SeqExp exps => 
                let
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
            | A.VarExp var => transVar (venv, tenv, var)
            | A.AssignExp {var, exp, pos} => let
                    val {exp=_, ty=tyvar} = transVar (venv, tenv, var)
                    val {exp=_, ty=tyexp} = transExp (venv, tenv, exp)
                in
                    if T.equals(tyvar, tyexp) then {exp=(), ty=tyvar}
                    else raise ErrorMsg.Error
                end
            | A.WhileExp {test, body, pos} => let
                    val {exp=_, ty=tytest} = transExp (venv, tenv, test)
                    val {exp=_, ty=tybody} = transExp (venv, tenv, body)
                in
                    if T.equals(tytest, T.INT) then ()
                    else raise ErrorMsg.Error;
                    {exp=(), ty=tybody}
                end
            | A.ForExp {var, escape, lo, hi, body, pos} => let
                    val {exp=_, ty=tylo} = transExp (venv, tenv, lo)
                    val {exp=_, ty=tyhi} = transExp (venv, tenv, hi)
                    val newVenv = Symbol.enter (venv, var, Env.VarEntry {ty=T.INT})
                    val {exp=_, ty=tybody} = transExp (newVenv, tenv, body)
                in          
                    PrintEnv.printEnv (newVenv, tenv);
                    if T.equals(tylo, T.INT) andalso T.equals(tyhi, T.INT) then ()
                    else raise ErrorMsg.Error;
                    {exp=(), ty=tybody}
                end
            | A.BreakExp _ => {exp=(), ty=T.NIL}
             (*TODO 0. check if func exist 1. check if args align with definition *)
            | A.CallExp {func, args, pos} => let
                    val funcentry = case Symbol.look (venv, func) of
                        NONE => raise ErrorMsg.Error
                        | SOME entry => entry
                    val {formals, result} = case funcentry of
                        Env.FunEntry record => record
                        | _ => raise ErrorMsg.Error
                    (*check if the number of args align with the number of formals*)
                    val _ = if (length args) = (length formals) then ()
                        else raise ErrorMsg.Error
                    val _ = map (fn (arg, formal) =>
                        let
                            val {exp=_, ty=tyarg} = transExp (venv, tenv, arg)
                        in
                            if T.equals(tyarg, formal) then ()
                            else raise ErrorMsg.Error
                        end) (ListPair.zip(args, formals))
                in
                    {exp=(), ty=result}
                end
            | A.RecordExp {fields, typ, pos} => {exp=(), ty=T.NIL} (*TODO*)
            | A.ArrayExp {typ, size, init, pos} => {exp=(), ty=T.NIL} (*TODO*)
            

    and transDec (venv : tyvenv, tenv: tytenv, dec : A.dec) = 
            case dec of
                (*To check a VarDec: 0. TODO: the proposed type exists 1. the type of init should be same as ty if there are ty 2. add Var to venv*)
                A.VarDec{name, escape, typ, init, pos}=> let
                        val {exp=_, ty=tyinit} = transExp (venv, tenv, init)
                        val newtyp = case typ of
                                NONE => tyinit
                            | SOME t => case Symbol.look (tenv, #1 t) of 
                                    NONE => raise ErrorMsg.Error
                                | SOME ty => ty
                    in
                        if T.equals(newtyp, tyinit) then 
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
                    val uniqueAndTydecMap : ((T.unique * A.tydec) Symbol.table) = foldl (fn (tydec, tab) => 
                        let
                            val {name=n, ty=t, pos=_} = tydec
                        in
                            case t of
                            A.RecordTy _ => Symbol.enter (tab, n, (ref (), tydec))
                            | _ => tab
                        end) Symbol.empty tydecs
                    fun makeRec tydec = 
                        let
                            val {name=n, ty=t, pos=pos} = tydec
                            val unique = case Symbol.look (uniqueAndTydecMap, n) of
                                SOME (u,_) => SOME u
                                | _ => NONE
                            val recordFields = case t of
                                A.RecordTy fields => foldr (fn (field, fieldlist) =>
                                    let
                                        val {name, escape, typ, pos} = field
                                        val recordTydec = case Symbol.look (uniqueAndTydecMap, typ) of
                                            SOME (_,rt) => SOME rt
                                            | _ => NONE
                                        val ty = case recordTydec of
                                            NONE => (case Symbol.look (tenv, typ) of
                                                NONE => (ErrorMsg.error pos ("Undefined type " ^ Symbol.name typ); NONE)
                                                | SOME ty => SOME ty)
                                            | _ => NONE
                                    in
                                        case recordTydec of
                                            SOME rt => ((name, T.RECORD (fn () => makeRec rt)) :: fieldlist)
                                            | _ => case ty of
                                                SOME t => ((name, t) :: fieldlist)
                                                | _ => fieldlist
                                    end) [] fields
                                | _ => []
                        in
                            case unique of
                                NONE => raise ErrorMsg.Error
                                | SOME u => (recordFields, u, n)
                        end
                    val (newvenv, newtenv) = foldl (fn (tydec, (venv, tenv)) => 
                        let 
                            val {name=n, ty=t, pos=_} = tydec
                        in
                            case t of
                                A.RecordTy _ => (venv, Symbol.enter (tenv, n, T.RECORD (fn () => makeRec tydec)))
                                | _ => transTyDec (venv, tenv, tydec)
                        end) (venv, tenv) tydecs
                in
                    PrintEnv.printEnv (newvenv, newtenv);
                    {venv=newvenv, tenv=newtenv}
                end
            | A.FunctionDec fundecs => 
                let
                    (*recursively check the signature of each function*)
                    val {venv=newvenv, tenv=newtenv} = foldl (fn (fundec, {venv, tenv}) => transFunDec (venv, tenv, fundec)) {venv=venv, tenv=tenv} fundecs
                    (*recursively check the body of each function*)
                    val _ = print "begin to check body of each function\n"
                    val _ = map (fn (fundec :Absyn.fundec) => (
                        let 
                        val {name, params, result, body, pos} = fundec
                        val {exp=bodyexp, ty=typ} = transExp (newvenv, newtenv, body)
                        (*check if expty consistent with the delared function ty*)
                        in
                        case result of
                        NONE => if (T.equals(typ, T.NIL)) then ()
                            else raise ErrorMsg.Error
                        | SOME (sym, _) => let 
                            val symty = case Symbol.look (newtenv, sym) of
                                NONE => raise ErrorMsg.Error
                                | SOME ty => ty
                            in
                            if (T.equals(typ, symty)) then ()
                                else raise ErrorMsg.Error
                            end
                        end)) fundecs
                in
                    PrintEnv.printEnv (newvenv, newtenv);
                    {venv=newvenv, tenv=newtenv}
                end
    and transVar (venv , tenv, var) = {exp = (), ty = T.INT} (*TODO: check if the variable defined*)
    and transTyDec (venv, tenv, tydec) = 
            let
                val {name=n, ty=t, pos=p} = tydec
            in 
                case t of
                    (* TODO: cross recursion check e.g. type a = b; type b = a & mutual recursive type def & self recursive type def*)
                    A.NameTy (nRefTo, p') => 
                        let
                            val t = case Symbol.look (tenv, nRefTo) of
                                NONE => (ErrorMsg.error p' ("Undefined type " ^ Symbol.name nRefTo); T.NIL)
                                | SOME ty => ty
                        in
                            (venv, Symbol.enter (tenv, n, t))
                        end
                    (*TODO: ArrayTy & RecordTy*)
                | A.ArrayTy (nRefTo, p') =>
                    let
                        val t = case Symbol.look (tenv, nRefTo) of
                            NONE => (ErrorMsg.error p' ("Undefined type " ^ Symbol.name nRefTo); T.NIL)
                            | SOME ty => ty
                    in
                        (venv, Symbol.enter (tenv, n, T.ARRAY (t, ref ())))
                    end
                | A.RecordTy fields => (venv, tenv)
            end
           
    and transFunDec (venv, tenv, fundec) = 
            (* fundec = {name: symbol, params: field list, result: (symbol * pos) option, body: exp pos: pos}*)
            (* TODO: recurse through body of fundec, handle recursive fundec*)
            let
                val {name, params, result, body, pos} = fundec
                val params_ty = []
                fun helper (param, params_ty) = 
                        let
                            val {name, escape, typ, pos} = param
                            val ty = case Symbol.look (tenv, typ) of
                                    NONE => raise ErrorMsg.Error
                                | SOME ty => ty
                        in
                            ty::params_ty
                        end
                val newparams_ty = foldr helper params_ty params
                val res_ty = case result of NONE => raise ErrorMsg.Error
                    | SOME (sym, _) => 
                        case Symbol.look (tenv, sym) of
                            NONE => raise ErrorMsg.Error
                        | SOME ty => ty
                val newVenv = Symbol.enter (venv, name, Env.FunEntry {formals=newparams_ty, result=res_ty}) 
            in
                PrintEnv.printEnv (newVenv, tenv);
                {venv=newVenv, tenv=tenv}
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