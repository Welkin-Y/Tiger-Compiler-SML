
(* dummy Translate *)
structure Translate = struct type exp = unit end
(*TODO: recursive name type e.g. type a = b; type b = int*)
structure Semant :> SEMANT =
struct
    type tyvenv = Env.enventry Symbol.table
    type tytenv = T.ty Symbol.table
    type expty = {exp: Translate.exp, ty: T.ty}
    
    fun transExp (venv : tyvenv, tenv: tytenv, exp: A.exp, loopDepth: int) = 
            case exp of 
                (*To check an IfExp we need to make sure: 1. test is int 2. then and else have same types*)
                A.IfExp {test, then', else', pos} => 
                    let 
                        val {exp=_, ty=tythen} = transExp (venv, tenv, then', loopDepth)
                        val {exp=expelse, ty=tyelse} = case else' of
                                NONE => {exp=(), ty=tythen}
                            | SOME e => transExp (venv, tenv, e, loopDepth)

                        val {exp=_, ty=tytest} = transExp (venv, tenv, test, loopDepth) 
                    in
                        case tytest of
                            T.INT => ()
                        | _ => (ErrorMsg.error pos "test expression should be int"; raise ErrorMsg.Error);
                        if not (T.equals(tythen, tyelse)) then (ErrorMsg.error pos (" Unmatched type: then exp is " ^ T.toString tythen ^ ", else exp is " ^ T.toString tyelse); 
                        raise ErrorMsg.Error)
                         else ();
                        {exp=(), ty=tythen}
                    end
            | A.OpExp {left, oper, right, pos} =>
                let
                    val {exp=_, ty=tyleft} = transExp (venv, tenv, left, loopDepth)
                    val {exp=_, ty=tyright} = transExp (venv, tenv, right, loopDepth)
                in
                    case oper of
                        A.PlusOp => if T.equals(tyleft, T.INT) andalso T.equals(tyright, T.INT) then {exp=(), ty=T.INT}
                            else (ErrorMsg.error pos ("PlusOp: expect int*int, but got " ^ T.toString tyleft ^ " and " ^ T.toString tyright); raise ErrorMsg.Error)
                    | A.MinusOp => if T.equals(tyleft, T.INT) andalso T.equals(tyright, T.INT) then {exp=(), ty=T.INT}
                        else (ErrorMsg.error pos ("MinusOp: expect int*int, but got " ^ T.toString tyleft ^ " and " ^ T.toString tyright); raise ErrorMsg.Error)
                    | A.TimesOp => if T.equals(tyleft, T.INT) andalso T.equals(tyright, T.INT) then {exp=(), ty=T.INT}
                        else (ErrorMsg.error pos ("TimesOp: expect int*int, but got " ^ T.toString tyleft ^ " and " ^ T.toString tyright); raise ErrorMsg.Error)
                    | A.DivideOp => if T.equals(tyleft, T.INT) andalso T.equals(tyright, T.INT) then {exp=(), ty=T.INT}
                        else (ErrorMsg.error pos ("DivideOp: expect int*int, but got " ^ T.toString tyleft ^ " and " ^ T.toString tyright); raise ErrorMsg.Error)
                    | A.EqOp => if T.equals(tyleft, tyright) then case tyleft of
                            T.INT => {exp=(), ty=T.INT}
                            | T.RECORD f => {exp=(), ty=T.RECORD f}
                            | T.ARRAY t => {exp=(), ty=T.ARRAY t}
                            | _ => (ErrorMsg.error pos ("EqOp: expect int or record or array, but got " ^ T.toString tyleft ^ " and " ^ T.toString tyright); raise ErrorMsg.Error)
                        else (ErrorMsg.error pos ("EqOp: expect same type, but got " ^ T.toString tyleft ^ " and " ^ T.toString tyright); raise ErrorMsg.Error)
                    | A.NeqOp => if T.equals(tyleft, tyright) then case tyleft of
                            T.INT => {exp=(), ty=T.INT}
                            | T.RECORD f => {exp=(), ty=T.RECORD f}
                            | T.ARRAY t => {exp=(), ty=T.ARRAY t}
                            | _ => (ErrorMsg.error pos ("NeqOp: expect int or record or array, but got " ^ T.toString tyleft ^ " and " ^ T.toString tyright); raise ErrorMsg.Error)
                        else (ErrorMsg.error pos ("NeqOp: expect same type, but got " ^ T.toString tyleft ^ " and " ^ T.toString tyright); raise ErrorMsg.Error)
                    | A.LtOp => if T.equals(tyleft, tyright) andalso T.equals(T.INT, tyleft) then {exp=(), ty=T.INT}
                        else (ErrorMsg.error pos ("LtOp: expect int*int, but got " ^ T.toString tyleft ^ " and " ^ T.toString tyright); raise ErrorMsg.Error)
                    | A.LeOp => if T.equals(tyleft, tyright) andalso T.equals(T.INT, tyleft) then {exp=(), ty=T.INT}
                        else (ErrorMsg.error pos ("LeOp: expect int*int, but got " ^ T.toString tyleft ^ " and " ^ T.toString tyright); raise ErrorMsg.Error)
                    | A.GtOp => if T.equals(tyleft, tyright) andalso T.equals(T.INT, tyleft) then {exp=(), ty=T.INT}
                        else (ErrorMsg.error pos ("GtOp: expect int*int, but got " ^ T.toString tyleft ^ " and " ^ T.toString tyright); raise ErrorMsg.Error)
                    | A.GeOp => if T.equals(tyleft, tyright) andalso T.equals(T.INT, tyleft) then {exp=(), ty=T.INT}
                        else (ErrorMsg.error pos ("GeOp: expect int*int, but got " ^ T.toString tyleft ^ " and " ^ T.toString tyright); raise ErrorMsg.Error)
                end
            (* Check Let Exp: 1. gothrough decs 2. go through body*)
            | A.LetExp {decs, body, pos} => 
                let
                    val {venv=venv', tenv=tenv'} = foldl (fn (dec, {venv, tenv}) => transDec (venv, tenv, dec, loopDepth)) {venv=venv, tenv=tenv} decs
                in
                    print ("begin to check body\n");
                    transExp (venv', tenv', body, loopDepth)
                end
            | A.IntExp _ => {exp=(), ty=T.INT}
            | A.StringExp _ => {exp=(), ty=T.STRING}
            | A.NilExp => {exp=(), ty=T.NIL}
            | A.SeqExp exps => 
                let
                    val explist : Translate.exp list = []
                    fun helper ((exp, _), ty) = let
                                val {exp=entryExp, ty=entryTy} = transExp (venv, tenv, exp, loopDepth)
                            in
                                explist = explist @ [entryExp];
                                entryTy
                            end
                in
                    {exp=(), ty=foldl helper T.UNIT exps}
                end
            | A.VarExp var => transVar (venv, tenv, var, loopDepth)
            | A.AssignExp {var, exp, pos} => let
                    val {exp=_, ty=tyvar} = transVar (venv, tenv, var, loopDepth)
                    val {exp=_, ty=tyexp} = transExp (venv, tenv, exp, loopDepth)
                in
                    if T.equals(tyvar, tyexp) then {exp=(), ty=tyvar}
                    else raise ErrorMsg.Error
                end
            | A.WhileExp {test, body, pos} => let
                    val {exp=_, ty=tytest} = transExp (venv, tenv, test, loopDepth)
                    val {exp=_, ty=tybody} = transExp (venv, tenv, body, loopDepth+1)
                in
                    if T.equals(tytest, T.INT) then ()
                    else (ErrorMsg.error pos "test expression should be int"; raise ErrorMsg.Error);
                    {exp=(), ty=tybody}
                end
            | A.ForExp {var, escape, lo, hi, body, pos} => let
                    val {exp=_, ty=tylo} = transExp (venv, tenv, lo, loopDepth)
                    val {exp=_, ty=tyhi} = transExp (venv, tenv, hi, loopDepth)
                    val newVenv = Symbol.enter (venv, var, Env.VarEntry {ty=T.INT})
                    val {exp=_, ty=tybody} = transExp (newVenv, tenv, body, loopDepth+1)
                in          
                    PrintEnv.printEnv (newVenv, tenv);
                    if T.equals(tylo, T.INT) andalso T.equals(tyhi, T.INT) then ()
                    else (ErrorMsg.error pos "lo and hi should be int"; raise ErrorMsg.Error);
                    {exp=(), ty=tybody}
                end
            | A.BreakExp pos => if loopDepth > 0 then {exp=(), ty=T.UNIT}
                else (ErrorMsg.error pos "break statement not within loop"; raise ErrorMsg.Error)
             (*0. check if func exist 1. check if args align with definition *)
            | A.CallExp {func, args, pos} => let
                    val funcentry = case Symbol.look (venv, func) of
                        NONE => (ErrorMsg.error pos ("Undefined function " ^ Symbol.name func); raise ErrorMsg.Error)
                        | SOME entry => entry
                    val {formals, result} = case funcentry of
                        Env.FunEntry record => record
                        | _ => raise ErrorMsg.Error
                    (*check if the number of args align with the number of formals*)
                    val _ = if (length args) = (length formals) then ()
                        else (ErrorMsg.error pos ("Unmatched number of args: function " ^ Symbol.name func ^ " has " ^ Int.toString (length formals) ^ " args, but called with " ^ Int.toString (length args)); raise ErrorMsg.Error)
                    val _ = map (fn (arg, formal) =>
                        let
                            val {exp=_, ty=tyarg} = transExp (venv, tenv, arg, loopDepth)
                        in
                            if T.equals(tyarg, formal) then ()
                            else (ErrorMsg.error pos ("Unmatched type: arg is " ^ T.toString tyarg ^ ", but formal is " ^ T.toString formal); raise ErrorMsg.Error)
                        end) (ListPair.zip(args, formals))
                in
                    {exp=(), ty=result}
                end
                (*0. check if the type of record exist in tenv 1. check if the field name&type aigned with definition*)
            | A.RecordExp {fields, typ, pos} => let
                    val ty = case Symbol.look (tenv, typ) of
                        NONE => raise ErrorMsg.Error
                        | SOME ty => ty
                    val genfun = case ty of
                        T.RECORD genfun => genfun
                        | _ => raise ErrorMsg.Error
                    val (recordFields, _, _) = genfun ()
                    (*check if the number of fields align with the number of recordFields*)
                    val _ = if (length fields) = (length recordFields) then ()
                        else raise ErrorMsg.Error
                        (*check if the field name&type can be found in definition*)
                    val _ = map (fn (field) =>
                        let
                            val (symbol, exp, pos) = field
                            val {exp=_, ty=tyexp} = transExp (venv, tenv, exp, loopDepth)
                            (*check if the field name can be found in definition*)
                            
                        in
                            case List.find (fn (name, _) => name = symbol) recordFields of
                                NONE => raise ErrorMsg.Error
                                | SOME (name, ty) => 
                                    if T.equals(tyexp, ty) then ()
                                    else (print("type error"^(Int.toString pos));raise ErrorMsg.Error)
                        end) fields
                    (* check if all recordFields can be find in field*)
                    val _ = map (fn (name, typ) =>
                        
                        case List.find (fn (symbol, exp, pos) => symbol = name) fields of
                            NONE => raise ErrorMsg.Error
                            | SOME (symbol, exp, pos) => let
                                val {exp=_, ty=tyexp} = transExp (venv, tenv, exp, loopDepth)
                            in
                                if T.equals(tyexp, typ) then ()
                                else raise ErrorMsg.Error
                            end) recordFields
                in
                    {exp=(), ty=ty}
                end
            
            
            | A.ArrayExp {typ, size, init, pos} =>  (*TODO*)
            (*0. check if the type of array exist in tenv 1. check if the type of init is same as the type of array*)
            let 
                val ty = case Symbol.look (tenv, typ) of
                    NONE => raise ErrorMsg.Error
                    | SOME ty => ty
                val {exp=_, ty=tyinit} = transExp (venv, tenv, init, loopDepth)

            in
                if T.equals(ty, tyinit) then {exp=(), ty=T.ARRAY (ty, ref ())}
                else raise ErrorMsg.Error
            end
            

    and transDec (venv : tyvenv, tenv: tytenv, dec : A.dec, loopDepth: int) = 
            case dec of
                (*To check a VarDec: 0. TODO: the proposed type exists 1. the type of init should be same as ty if there are ty 2. add Var to venv*)
                A.VarDec{name, escape, typ, init, pos}=> let
                        val {exp=_, ty=tyinit} = transExp (venv, tenv, init, loopDepth)
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
                        else case newtyp of
                            T.NIL => (ErrorMsg.error pos ("Using nil to initialize variable without type"); raise ErrorMsg.Error)
                        | _ => (ErrorMsg.error pos ("Unmatched type: var " ^ Symbol.name name ^ " is " ^ T.toString tyinit ^ ", but declared as " ^ T.toString newtyp); raise ErrorMsg.Error)
                    end
            | A.TypeDec tydecs => 
                let
                    val newvenv : (Env.enventry Symbol.table) ref = ref venv
                    val newtenv : (T.ty Symbol.table) ref = ref tenv
                    val aliasTable : ((unit Symbol.table) Symbol.table) ref = ref Symbol.empty
                    fun updateAlias (name, ty) = 
                        case Symbol.look (!aliasTable, name) of
                            SOME alias => Symbol.appi (fn (n, _) => (newtenv := Symbol.enter (!newtenv, n, ty); updateAlias (n, ty))) alias
                            | NONE => ()
                    val tydecTable : ((A.tydec * T.unique) Symbol.table) = foldl (fn (tydec, tab) => 
                        let
                            val {name=name, ty=ty, pos=pos} = tydec
                        in
                            case ty of
                                A.NameTy (n, p) => case Symbol.look (!newtenv, n) of
                                    NONE => (* TODO *)
                                    | SOME ty => (
                                        newtenv := Symbol.enter (newtenv, name, ty);
                                        case Symbol.look (!aliasTable, name) of
                                            NONE => aliasTable := Symbol.enter (!aliasTable, name, Symbol.empty)
                                            | SOME _ => (* TODO *)
                                    )
                        end) Symbol.empty tydecs
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
                                | _ => transTyDec (venv, tenv, tydec, loopDepth)
                        end) (venv, tenv) tydecs
                in
                    PrintEnv.printEnv (newvenv, newtenv);
                    {venv=newvenv, tenv=newtenv}
                end
            | A.FunctionDec fundecs => 
                let
                    (*recursively check the signature of each function*)
                    val {venv=newvenv, tenv=newtenv} = foldl (fn (fundec, {venv, tenv}) => transFunDec (venv, tenv, fundec, loopDepth)) {venv=venv, tenv=tenv} fundecs
                    (*recursively check the body of each function*)
                    val _ = print "begin to check body of each function\n"

                    val _ = map (fn (fundec :Absyn.fundec) => (
                        let 
                        val {name, params, result, body, pos} = fundec
                        (* add all args var into a tmp venv to check body*)
                        val tmpvenv = foldl (fn (param, venv) => 
                            let
                                val {name, escape, typ, pos} = param
                                val ty = case Symbol.look (tenv, typ) of
                                    NONE => raise ErrorMsg.Error
                                | SOME ty => ty
                            in
                                Symbol.enter (venv, name, Env.VarEntry {ty=ty})
                            end) newvenv params
                        val {exp=bodyexp, ty=typ} = transExp (tmpvenv, newtenv, body, 0)
                        (*check if expty consistent with the delared function ty*)
                        in
                        case result of
                        NONE => if (T.equals(typ, T.UNIT)) then ()
                            else ( ErrorMsg.error pos ("Unmatched type: function " ^ Symbol.name name ^ " return type is " ^ T.toString typ ^ ", but declared as unit"); raise ErrorMsg.Error) 
                        | SOME (sym, _) => let 
                            val symty = case Symbol.look (newtenv, sym) of
                                NONE => (ErrorMsg.error pos ("Undefined type " ^ Symbol.name sym); raise ErrorMsg.Error)
                                | SOME ty => ty
                            in
                            if (T.equals(typ, symty)) then ()
                                else ( ErrorMsg.error pos ("Unmatched type: function " ^ Symbol.name name ^ " return type is " ^ T.toString typ ^ ", but declared as " ^ Symbol.name sym ^ " type " ^ T.toString symty); raise ErrorMsg.Error)
                            end
                        end)) fundecs
                in
                    PrintEnv.printEnv (newvenv, newtenv);
                    {venv=newvenv, tenv=newtenv}
                end
    and transVar (venv:tyvenv , tenv:tytenv, var: A.var, loopDepth: int) = let 
            val {exp=_, ty=ty} = case var of
                A.SimpleVar (n, p) => 
                    (case Symbol.look (venv, n) of
                        NONE => raise ErrorMsg.Error
                        | SOME entry => case entry of
                            Env.VarEntry {ty=ty} => {exp=(), ty=ty}
                            | _ => raise ErrorMsg.Error)
                | A.FieldVar (var, n, p) =>
                    let
                        val {exp=_, ty=ty} = transVar (venv, tenv, var, loopDepth)
                        val genfun = case ty of
                            T.RECORD genfun => genfun
                            | _ => raise ErrorMsg.Error
                        val (fields, _, _) = genfun ()
                    in
                        case List.find (fn (name, _) => name = n) fields of
                            NONE => raise ErrorMsg.Error
                            | SOME (_, ty) => {exp=(), ty=ty}
                    end
                | A.SubscriptVar (var, exp, p) => 
                    let
                        val {exp=_, ty=ty} = transVar (venv, tenv, var, loopDepth)
                        val {exp=_, ty=tyexp} = transExp (venv, tenv, exp, loopDepth)
                    in
                        if T.equals(tyexp, T.INT) then ()
                        else raise ErrorMsg.Error;
                        {exp=(), ty=ty}
                    end

        in
            {exp = (), ty = ty}
        end 
    and transTyDec (venv:tyvenv, tenv:tytenv, tydec:A.tydec, loopDepth: int) = 
            let
                val {name=n, ty=t, pos=p} = tydec
            in 
                case t of
                    A.NameTy (nRefTo, p') => 
                        let
                            val t = case Symbol.look (tenv, nRefTo) of
                                NONE => (ErrorMsg.error p' ("Undefined type " ^ Symbol.name nRefTo);
                                raise ErrorMsg.Error)
                                | SOME ty => ty
                        in
                            (venv, Symbol.enter (tenv, n, t))
                        end
                    (*ArrayTy*)
                | A.ArrayTy (nRefTo, p') =>
                    let
                        val t = case Symbol.look (tenv, nRefTo) of
                            NONE => (ErrorMsg.error p' ("Undefined type " ^ Symbol.name nRefTo); raise ErrorMsg.Error)
                            | SOME ty => ty
                    in
                        (venv, Symbol.enter (tenv, n, T.ARRAY (t, ref ())))
                    end
                | A.RecordTy fields => (venv, tenv)
            end
           
    and transFunDec (venv:tyvenv, tenv:tytenv, fundec:A.fundec, loopDepth: int) = 
            (* fundec = {name: symbol, params: field list, result: (symbol * pos) option, body: exp pos: pos}*)
            (* recurse through body of fundec, handle recursive fundec*)
            let
                val {name, params, result, body, pos} = fundec
                val params_ty = []
                fun helper (param, params_ty) = 
                        let
                            val {name, escape, typ, pos} = param
                            val ty = case Symbol.look (tenv, typ) of
                                    NONE => ( ErrorMsg.error pos ("Undefined type " ^ Symbol.name typ); raise ErrorMsg.Error)
                                | SOME ty => ty
                        in
                            ty::params_ty
                        end
                val newparams_ty = foldr helper params_ty params
                val res_ty = case result of NONE => T.UNIT
                    | SOME (sym, _) => 
                        case Symbol.look (tenv, sym) of
                            NONE => raise ErrorMsg.Error
                        | SOME ty => ty
                val newVenv = Symbol.enter (venv, name, Env.FunEntry {formals=newparams_ty, result=res_ty}) 
            in
                PrintEnv.printEnv (newVenv, tenv);
                {venv=newVenv, tenv=tenv}
            end
    and transTy (tenv, ty) = T.INT (*TODO*)
    and transProg (exp: A.exp) = (
                let 
                    val venv = Env.base_venv
                    val tenv = Env.base_tenv 
                in

                    transExp (venv, tenv, exp, 0);
                    PrintEnv.printEnv (venv, tenv)
                end;
                print "\nSemantic Analysis Succeed\n"
            )
end