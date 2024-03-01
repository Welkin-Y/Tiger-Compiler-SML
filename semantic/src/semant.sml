
(* dummy Translate *)
structure Translate = struct type exp = unit end
structure TC = TypeChecker
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
                        TC.checkIfExp pos (tytest, tythen, tyelse);
                        {exp=(), ty=tythen}
                    end
            | A.OpExp {left, oper, right, pos} =>
                let
                    val {exp=_, ty=tyleft} = transExp (venv, tenv, left, loopDepth)
                    val {exp=_, ty=tyright} = transExp (venv, tenv, right, loopDepth)
                in
                    case oper of
                        A.PlusOp => (TC.checkIntOp oper pos (tyleft, tyright);{exp=(), ty=T.INT})
                    | A.MinusOp => (TC.checkIntOp oper pos (tyleft, tyright);{exp=(), ty=T.INT})
                    | A.TimesOp => (TC.checkIntOp oper pos (tyleft, tyright);{exp=(), ty=T.INT})
                    | A.DivideOp => (TC.checkIntOp oper pos (tyleft, tyright);{exp=(), ty=T.INT})
                    | A.EqOp => (TC.checkEqOp oper pos (tyleft, tyright);{exp=(), ty=T.INT})
                    | A.NeqOp => (TC.checkEqOp oper pos (tyleft, tyright);{exp=(), ty=T.INT})
                    | A.LtOp => (TC.checkIntOp oper pos (tyleft, tyright);{exp=(), ty=T.INT})
                    | A.LeOp => (TC.checkIntOp oper pos (tyleft, tyright);{exp=(), ty=T.INT})
                    | A.GtOp => (TC.checkIntOp oper pos (tyleft, tyright);{exp=(), ty=T.INT})
                    | A.GeOp => (TC.checkIntOp oper pos (tyleft, tyright);{exp=(), ty=T.INT})
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
                    TC.checkSameType pos (tyvar, tyexp);
                    {exp=(), ty=T.UNIT}
                end
            | A.WhileExp {test, body, pos} => let
                    val {exp=_, ty=tytest} = transExp (venv, tenv, test, loopDepth)
                    val {exp=_, ty=tybody} = transExp (venv, tenv, body, loopDepth+1)
                in
                    TC.checkIsType pos (tytest, T.INT);
                    {exp=(), ty=tybody}
                end
            | A.ForExp {var, escape, lo, hi, body, pos} => let
                    val {exp=_, ty=tylo} = transExp (venv, tenv, lo, loopDepth)
                    val {exp=_, ty=tyhi} = transExp (venv, tenv, hi, loopDepth)
                    val newVenv = Symbol.enter (venv, var, Env.VarEntry {ty=T.INT})
                    val {exp=_, ty=tybody} = transExp (newVenv, tenv, body, loopDepth+1)
                in          
                    PrintEnv.printEnv (newVenv, tenv);
                    TC.checkIsType pos (tylo, T.INT);
                    TC.checkIsType pos (tyhi, T.INT);
                    {exp=(), ty=tybody}
                end
            | A.BreakExp pos => if loopDepth > 0 then {exp=(), ty=T.UNIT}
                else (ErrorMsg.error pos "SyntaxError: break outside loop"; raise ErrorMsg.Error)
             (*0. check if func exist 1. check if args align with definition *)
            | A.CallExp {func, args, pos} => let
                    val funcentry = case Symbol.look (venv, func) of
                        NONE => TC.undefinedNameErr pos func
                        | SOME entry => entry
                    val {formals, result} = case funcentry of
                        Env.FunEntry record => record
                        | _ => (ErrorMsg.error pos ("SyntaxError: not a function " ^ Symbol.name func); raise ErrorMsg.Error)
                    (*check if the number of args align with the number of formals*)
                    val _ = if (length args) = (length formals) then ()
                        else (ErrorMsg.error pos ("TypeError: " ^ Symbol.name func ^ " () takes " ^ Int.toString (length formals) ^ " positional argument(s) but called with" ^ Int.toString (length args) ^ " argument(s)"); raise ErrorMsg.Error)
                    val _ = map (fn (arg, formal) =>
                        let
                            val {exp=_, ty=tyarg} = transExp (venv, tenv, arg, loopDepth)
                        in
                            TC.checkSameType pos (tyarg, formal)
                        end) (ListPair.zip(args, formals))
                in
                    {exp=(), ty=result}
                end
                (*0. check if the type of record exist in tenv 1. check if the field name&type aigned with definition*)
            | A.RecordExp {fields, typ, pos} => let
                    val ty = case Symbol.look (tenv, typ) of
                        NONE => TC.undefinedTypeErr pos typ
                        | SOME ty => ty
                    val genfun = case ty of
                        T.RECORD genfun => genfun
                        | _ => (ErrorMsg.error pos ("TypeError: not a record type " ^ Symbol.name typ); raise ErrorMsg.Error)
                    val (recordFields, _, _) = genfun ()
                    (*check if the number of fields align with the number of recordFields*)
                    val _ = if (length fields) = (length recordFields) then ()
                        else (ErrorMsg.error pos ("TypeError: unmatched number of fields: record " ^ Symbol.name typ ^ " has " ^ Int.toString (length recordFields) ^ " fields, but called with " ^ Int.toString (length fields)); raise ErrorMsg.Error)
                        (*check if the field name&type can be found in definition*)
                    val _ = map (fn (field) =>
                        let
                            val (symbol, exp, pos) = field
                            val {exp=_, ty=tyexp} = transExp (venv, tenv, exp, loopDepth)
                            (*check if the field name can be found in definition*)
                            
                        in
                            case List.find (fn (name, _) => name = symbol) recordFields of
                                NONE => TC.undefinedNameErr pos symbol
                                | SOME (name, ty) => 
                                    TC.checkSameType pos (tyexp, ty)
                        end) fields
                    (* check if all recordFields can be find in field*)
                    val _ = map (fn (name, typ) =>
                        
                        case List.find (fn (symbol, exp, pos) => symbol = name) fields of
                            NONE => TC.undefinedNameErr pos name
                            | SOME (symbol, exp, pos) => let
                                val {exp=_, ty=tyexp} = transExp (venv, tenv, exp, loopDepth)
                            in
                                TC.checkSameType pos (tyexp, typ)
                            end) recordFields
                in
                    {exp=(), ty=ty}
                end
            
            
            | A.ArrayExp {typ, size, init, pos} =>  (*TODO*)
            (*0. check if the type of array exist in tenv 1. check if the type of init is same as the type of array*)
            let 
                val ty = case Symbol.look (tenv, typ) of
                    NONE => TC.undefinedTypeErr pos typ
                    | SOME ty => ty
                val {exp=_, ty=tyinit} = transExp (venv, tenv, init, loopDepth)
            in  
                case ty of T.ARRAY (ty',_) => (TC.checkSameType pos (ty', tyinit); {exp=(), ty=ty})
                | _ => (ErrorMsg.error pos ("TypeError: not an array type " ^ Symbol.name typ); raise ErrorMsg.Error)
            end
            

    and transDec (venv : tyvenv, tenv: tytenv, dec : A.dec, loopDepth: int) = 
            case dec of
                (*To check a VarDec: 0. TODO: the proposed type exists 1. the type of init should be same as ty if there are ty 2. add Var to venv*)
                A.VarDec{name, escape, typ, init, pos}=> let
                        val {exp=_, ty=tyinit} = transExp (venv, tenv, init, loopDepth)
                        val newtyp = case typ of
                                NONE => tyinit
                            | SOME t => case Symbol.look (tenv, #1 t) of 
                                    NONE => TC.undefinedTypeErr pos (#1 t)
                                | SOME ty => ty
                    in
                        if T.equals(newtyp, tyinit) then 
                            let 
                                val newVenv =  Symbol.enter(venv, name, Env.VarEntry {ty=newtyp})
                                val _ = PrintEnv.printEnv (newVenv,tenv)
                            in
                                {venv = newVenv, tenv = tenv}
                            end
                        else case newtyp of
                            T.NIL => (ErrorMsg.error pos ("SyntaxError: using nil to initialize variable without type"); raise ErrorMsg.Error)
                        | _ => (ErrorMsg.error pos ("TypeError: unmatched type: var " ^ Symbol.name name ^ " is " ^ T.toString tyinit ^ ", but declared as " ^ T.toString newtyp); raise ErrorMsg.Error)
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
                                                NONE => TC.undefinedTypeErr pos typ
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
                                NONE => TC.undefinedNameErr pos n
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
                                    NONE => TC.undefinedTypeErr pos typ
                                | SOME ty => ty
                            in
                                Symbol.enter (venv, name, Env.VarEntry {ty=ty})
                            end) newvenv params
                        val {exp=bodyexp, ty=typ} = transExp (tmpvenv, newtenv, body, 0)
                        (*check if expty consistent with the delared function ty*)
                        in
                        case result of
                        NONE => TC.checkIsType pos (typ, T.UNIT)
                        | SOME (sym, _) => let 
                            val symty = case Symbol.look (newtenv, sym) of
                                NONE => TC.undefinedTypeErr pos sym
                                | SOME ty => ty
                            in
                                TC.checkSameType pos (typ, symty)
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
                        NONE => TC.undefinedNameErr p n
                        | SOME entry => case entry of
                            Env.VarEntry {ty=ty} => {exp=(), ty=ty}
                            | _ => (ErrorMsg.error p ("TypeError: not a variable " ^ Symbol.name n); raise ErrorMsg.Error))
                | A.FieldVar (var, n, p) =>
                    let
                        val {exp=_, ty=ty} = transVar (venv, tenv, var, loopDepth)
                        val genfun = case ty of
                            T.RECORD genfun => genfun
                            | _ => (ErrorMsg.error p ("TypeError: not a record type " ^ T.toString ty); raise ErrorMsg.Error)
                        val (fields, _, _) = genfun ()
                    in
                        case List.find (fn (name, _) => name = n) fields of
                            NONE => TC.undefinedNameErr p n
                            | SOME (_, ty) => {exp=(), ty=ty}
                    end
                | A.SubscriptVar (var, exp, p) => 
                    let
                        val {exp=_, ty=ty} = transVar (venv, tenv, var, loopDepth)
                        val {exp=_, ty=tyexp} = transExp (venv, tenv, exp, loopDepth)
                    in
                        TC.checkIsType p (tyexp, T.INT);
                        case ty of 
                            T.ARRAY (ty', _) => {exp=(), ty=ty'}
                            | _ => (ErrorMsg.error p ("TypeError: not an array type " ^ T.toString ty); raise ErrorMsg.Error)
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
                                NONE => TC.undefinedTypeErr p' nRefTo
                                | SOME ty => ty
                        in
                            (venv, Symbol.enter (tenv, n, t))
                        end
                    (*ArrayTy*)
                | A.ArrayTy (nRefTo, p') =>
                    let
                        val t = case Symbol.look (tenv, nRefTo) of
                            NONE => TC.undefinedTypeErr p' nRefTo
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
                                    NONE => TC.undefinedTypeErr pos typ
                                | SOME ty => ty
                        in
                            ty::params_ty
                        end
                val newparams_ty = foldr helper params_ty params
                val res_ty = case result of NONE => T.UNIT
                    | SOME (sym, _) => 
                        case Symbol.look (tenv, sym) of
                            NONE => TC.undefinedTypeErr pos sym
                        | SOME ty => ty
                val newVenv = Symbol.enter (venv, name, Env.FunEntry {formals=newparams_ty, result=res_ty}) 
            in
                PrintEnv.printEnv (newVenv, tenv);
                {venv=newVenv, tenv=tenv}
            end
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