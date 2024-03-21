structure Semant :> SEMANT =
struct
    structure TL = Translate
    structure TC = TypeChecker

    type tyvenv = Env.enventry Symbol.table
    type tytenv = T.ty Symbol.table
    type expty = {exp: TL.exp, ty: T.ty}
    
    
    fun transExp (venv : tyvenv, tenv: tytenv, exp: A.exp, loopDepth: int, forbidden : (Symbol.symbol list), level: TL.level) = 
            case exp of 
            (*To check an IfExp we need to make sure: 1. test is int 2. then and else have same types*)
            A.IfExp {test, then', else', pos} => let
                    val {exp=expthen, ty=tythen} = transExp (venv, tenv, then', loopDepth, forbidden, level)
                    val {exp=exptest, ty=tytest} = transExp (venv, tenv, test, loopDepth, forbidden, level)
                in 
                    case else' of SOME elseexp => let 
                            val {exp=expelse, ty=tyelse} = transExp (venv, tenv, elseexp, loopDepth, forbidden, level)
                        in
                            TC.checkIfExp pos (tytest, tythen, tyelse);
                            {exp=TL.transIf(exptest, expthen, SOME(expelse)), ty=tythen}
                        end
                    | NONE => let in
                            TC.checkIsType pos (tytest, T.INT);
                            {exp=TL.transIf(exptest, expthen, NONE), ty=tythen}
                        end
                end
            | A.OpExp {left, oper, right, pos} =>
                let
                    val {exp=expleft, ty=tyleft} = transExp (venv, tenv, left, loopDepth, forbidden, level)
                    val {exp=expright, ty=tyright} = transExp (venv, tenv, right, loopDepth, forbidden, level)
                in
                    case oper of
                    A.PlusOp => (TC.checkIntStringOp oper pos (tyleft, tyright);{exp=TL.transBinop(oper, expleft, expright), ty=T.INT})
                    | A.MinusOp => (TC.checkIntStringOp oper pos (tyleft, tyright);{exp=TL.transBinop(oper, expleft, expright), ty=T.INT})
                    | A.TimesOp => (TC.checkIntStringOp oper pos (tyleft, tyright);{exp=TL.transBinop(oper, expleft, expright), ty=T.INT})
                    | A.DivideOp => (TC.checkIntStringOp oper pos (tyleft, tyright);{exp=TL.transBinop(oper, expleft, expright), ty=T.INT})
                    | A.EqOp => (TC.checkEqOp oper pos (tyleft, tyright);{exp=TL.transRelop(oper, expleft, expright), ty=T.INT})
                    | A.NeqOp => (TC.checkEqOp oper pos (tyleft, tyright);{exp=TL.transRelop(oper, expleft, expright), ty=T.INT})
                    | A.LtOp => (TC.checkIntStringOp oper pos (tyleft, tyright);{exp=TL.transRelop(oper, expleft, expright), ty=T.INT})
                    | A.LeOp => (TC.checkIntStringOp oper pos (tyleft, tyright);{exp=TL.transRelop(oper, expleft, expright), ty=T.INT})
                    | A.GtOp => (TC.checkIntStringOp oper pos (tyleft, tyright);{exp=TL.transRelop(oper, expleft, expright), ty=T.INT})
                    | A.GeOp => (TC.checkIntStringOp oper pos (tyleft, tyright);{exp=TL.transRelop(oper, expleft, expright), ty=T.INT})
                end
            (* Check Let Exp: 1. gothrough decs 2. go through body*)
            | A.LetExp {decs, body, pos} => 
                let
                    val {venv=venv', tenv=tenv', forbidden=forbidden'} = foldl (fn (dec, {venv, tenv, forbidden}) => 
                    let
                        val forbidden = case dec of 
                        A.VarDec {name, escape, typ, init, pos} => (
                            case List.find (fn n => n = name) forbidden of
                            NONE => forbidden
                            | SOME _ => List.filter (fn n => n <> name) forbidden)
                        | _ => forbidden
                        val {venv=venv, tenv=tenv} = transDec (venv, tenv, dec, loopDepth, forbidden, level)
                    in
                        {venv=venv, tenv=tenv, forbidden=forbidden}
                    end) {venv=venv, tenv=tenv, forbidden=forbidden} decs
                in
                    print ("begin to check body\n");
                    transExp (venv', tenv', body, loopDepth, forbidden', level)
                end
            | A.IntExp intval => {exp=TL.transInt intval, ty=T.INT}
            | A.StringExp (str, _) => {exp=TL.transString str, ty=T.STRING}
            | A.NilExp => {exp=TL.transNil(), ty=T.NIL}
            | A.SeqExp exps => 
                let
                    (* TODO *)
                    val explist : TL.exp list = []
                    fun helper ((exp, _), ty) = let
                                val {exp=entryExp, ty=entryTy} = transExp (venv, tenv, exp, loopDepth, forbidden, level)
                            in
                                
                                entryTy
                            end
                in
                    {exp=TL.NOT_IMPLEMENTED, ty=foldl helper T.UNIT exps}
                end
            | A.VarExp var => transVar (venv, tenv, var, loopDepth, forbidden, level)
            | A.AssignExp {var : A.var, exp: A.exp, pos :A.pos} => let
                    (*if var name in forbidden, raise error*)
                    val _ = case var of
                        A.SimpleVar (name, pos) => (case List.find (fn n => n = name) forbidden of
                            NONE => ()
                            | SOME _ => (ErrorMsg.error pos ("TypeError: reassign to for loop variable " ^ Symbol.name (name)); raise ErrorMsg.Error) )
                        | _ => ()
                    val {exp=varexp, ty=tyvar} = transVar (venv, tenv, var, loopDepth, forbidden, level)
                    val {exp=expexp, ty=tyexp} = transExp (venv, tenv, exp, loopDepth, forbidden, level)
                in
                    TC.checkSameType pos (tyvar, tyexp);
                    {exp=TL.transAssign(varexp, expexp), ty=T.UNIT}
                end
            | A.WhileExp {test, body, pos} => let
                    val {exp=_, ty=tytest} = transExp (venv, tenv, test, loopDepth, forbidden, level)
                    val {exp=_, ty=tybody} = transExp (venv, tenv, body, loopDepth+1, forbidden, level)
                in
                    TC.checkIsType pos (tytest, T.INT);
                    TC.checkIsType pos (tybody, T.UNIT);
                    {exp=TL.NOT_IMPLEMENTED, ty=T.UNIT}
                end
            | A.ForExp {var, escape, lo, hi, body, pos} => let
                    val {exp=_, ty=tylo} = transExp (venv, tenv, lo, loopDepth, forbidden, level)
                    val {exp=_, ty=tyhi} = transExp (venv, tenv, hi, loopDepth, forbidden, level)
                    (*if the id in forbidden raise error*)
                    val forbidden = case List.find (fn n => n = var) forbidden of
                        NONE => var::forbidden
                        | SOME _ => (ErrorMsg.error pos ("TypeError: reassign to for loop variable name " ^ Symbol.name var); raise ErrorMsg.Error)
                    val newVenv = Symbol.enter (venv, var, Env.VarEntry {access=TL.allocLocal level (!escape), ty=T.INT})
                    val {exp=_, ty=tybody} = transExp (newVenv, tenv, body, loopDepth+1, forbidden, level)
                in          
                    PrintEnv.printEnv (newVenv, tenv);
                    TC.checkIsType pos (tylo, T.INT);
                    TC.checkIsType pos (tyhi, T.INT);
                    TC.checkIsType pos (tybody, T.UNIT);
                    {exp=TL.NOT_IMPLEMENTED, ty=T.UNIT}
                end
            | A.BreakExp pos => if loopDepth > 0 then {exp=TL.NOT_IMPLEMENTED, ty=T.UNIT}
                else (ErrorMsg.error pos "SyntaxError: break outside loop"; raise ErrorMsg.Error)
             (*0. check if func exist 1. check if args align with definition *)
            | A.CallExp {func, args, pos} => let
                    val funcentry = case Symbol.look (venv, func) of
                        NONE => TC.undefinedNameErr pos func
                        | SOME entry => entry
                    val {formals=formals, result=result, ...} = case funcentry of
                        Env.FunEntry record => record
                        | _ => (ErrorMsg.error pos ("SyntaxError: not a function " ^ Symbol.name func); raise ErrorMsg.Error)
                    (*check if the number of args align with the number of formals*)
                    val _ = if (length args) = (length formals) then ()
                        else (ErrorMsg.error pos ("TypeError: " ^ Symbol.name func ^ " () takes " ^ Int.toString (length formals) ^ " positional argument(s) but called with" ^ Int.toString (length args) ^ " argument(s)"); raise ErrorMsg.Error)
                    val _ = map (fn (arg, formal) =>
                        let
                            val {exp=_, ty=tyarg} = transExp (venv, tenv, arg, loopDepth, forbidden, level)
                        in
                            TC.checkSameType pos (tyarg, formal)
                        end) (ListPair.zip(args, formals))
                in
                    {exp=TL.NOT_IMPLEMENTED, ty=result}
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
                            val {exp=_, ty=tyexp} = transExp (venv, tenv, exp, loopDepth, forbidden, level)
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
                                val {exp=_, ty=tyexp} = transExp (venv, tenv, exp, loopDepth, forbidden, level)
                            in
                                TC.checkSameType pos (tyexp, typ)
                            end) recordFields
                in
                    {exp=TL.NOT_IMPLEMENTED, ty=ty}
                end
            
            
            | A.ArrayExp {typ, size, init, pos} =>  (*TODO*)
            (*0. check if the type of array exist in tenv 1. check if the type of init is same as the type of array*)
            let 
                val ty = case Symbol.look (tenv, typ) of
                    NONE => TC.undefinedTypeErr pos typ
                    | SOME ty => ty
                val {exp=_, ty=tyinit} = transExp (venv, tenv, init, loopDepth, forbidden, level)
            in  
                case ty of T.ARRAY (ty',_) => (TC.checkSameType pos (ty', tyinit); {exp=TL.NOT_IMPLEMENTED, ty=ty})
                | _ => (ErrorMsg.error pos ("TypeError: not an array type " ^ Symbol.name typ); raise ErrorMsg.Error)
            end
            

    and transDec (venv : tyvenv, tenv: tytenv, dec : A.dec, loopDepth: int, forbidden : (Symbol.symbol list), level: TL.level) = 
            case dec of
                (*if declared variable has same name with forbidden, erase it from forbidden*)
                A.VarDec{name, escape, typ, init, pos}=> let

                        val {exp=_, ty=tyinit} = transExp (venv, tenv, init, loopDepth, forbidden, level)
                        val newtyp = case typ of
                                NONE => tyinit
                            | SOME t => case Symbol.look (tenv, #1 t) of 
                                    NONE => TC.undefinedTypeErr pos (#1 t)
                                | SOME ty => ty
                    in
                        if T.equals(newtyp, tyinit) then 
                            let 
                                val newVenv =  Symbol.enter(venv, name, Env.VarEntry {access=TL.allocLocal level (!escape), ty=newtyp})
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
                    (*check if there are duplicated field names in the same record type*)
                    val _ = map (fn (tydec) => 
                        let
                            val {name, ty, pos} = tydec
                        in
                            case ty of 
                                A.RecordTy fields => let
                                    val fieldNames = []
                                    fun helper (field, fieldNames) = 
                                        let
                                            val {name, escape, typ, pos} = field
                                        in
                                            case List.find (fn n => n = Symbol.name name) fieldNames of
                                                NONE => ((Symbol.name name)::fieldNames)
                                                | SOME _ => (ErrorMsg.error pos ("TypeError: reused field name " ^ Symbol.name name); raise ErrorMsg.Error)
                                        end
                                in  
                                foldr helper fieldNames fields 
                                end
                                | _ => []
                        end) tydecs 
                    val newvenv : (Env.enventry Symbol.table) ref = ref venv
                    val newtenv : (T.ty Symbol.table) ref = ref tenv
                    val aliasTable : ((unit Symbol.table) Symbol.table) ref = ref Symbol.empty
                    val tyTable : ((A.tydec * T.unique) Symbol.table) ref = ref Symbol.empty
                    fun updateAlias (name, ty) = 
                        case Symbol.look (!tyTable, name) of
                            NONE => ()
                            | SOME _ => (
                                newtenv := Symbol.enter (!newtenv, name, ty);
                                tyTable := #1 (Symbol.remove (!tyTable, name));
                                case Symbol.look (!aliasTable, name) of
                                    SOME alias => Symbol.appi (fn (n, _) => updateAlias (n, ty)) alias
                                    | NONE => ()
                            )
                    fun updateAliasRecAndArr (name, tyNu) = (
                        tyTable := Symbol.enter (!tyTable, name, tyNu);
                        case Symbol.look (!aliasTable, name) of
                            NONE => ()
                            | SOME alias => Symbol.appi (fn (n, _) => updateAliasRecAndArr (n, tyNu)) alias
                    )
                    val reuseChecker = foldl (fn (tydec, table) =>
                        let
                            val {name=name, ty=_, pos=pos} = tydec
                        in
                            case Symbol.look (table, name) of
                                NONE => Symbol.enter (table, name, ())
                                | SOME _ => (TC.reusedNameErr pos name; table)
                        end) Symbol.empty tydecs
                    val _ = foldl (fn (tydec, _) => 
                        let
                            val {name=name, ty=ty, pos=pos} = tydec
                        in
                            case ty of
                                A.NameTy (n, p) => (
                                    tyTable := Symbol.enter (!tyTable, name, (tydec, ref ()));
                                    case Symbol.look (!newtenv, n) of
                                        NONE => (case Symbol.look (!tyTable, n) of
                                            NONE => 
                                                aliasTable := Symbol.enter (!aliasTable, n, Symbol.enter (case Symbol.look (!aliasTable, n) of
                                                    SOME aT => aT
                                                    | NONE => Symbol.empty, name, ()))
                                            | SOME tyNu => 
                                                (let
                                                    val (tydec, unique) = tyNu
                                                    val {name=n, ty=t, pos=pos} = tydec
                                                in
                                                    case t of 
                                                        A.NameTy _ => TC.undefinedTypeErr pos n
                                                        | _ => updateAliasRecAndArr (name, tyNu)
                                                end))
                                        | SOME ty => updateAlias (name, ty))
                                | _ => updateAliasRecAndArr (name, (tydec, ref ()))
                        end) () tydecs
                    fun makeRec (tydec, unique) = 
                        let
                            val {name=n, ty=t, pos=pos} = tydec
                            val recordFields = case t of
                                A.RecordTy fields => foldr (fn (field, fieldlist) =>
                                    let
                                        val {name, escape, typ, pos} = field
                                        val tyNu = case Symbol.look (!tyTable, typ) of
                                            SOME tyNu => SOME tyNu
                                            | _ => NONE
                                        val ty = case tyNu of
                                            NONE => (case Symbol.look (!newtenv, typ) of
                                                NONE => (TC.undefinedTypeErr pos typ; NONE)
                                                | SOME ty => SOME ty)
                                            | _ => NONE
                                    in
                                        case tyNu of
                                            SOME tyNu => let
                                                val tydec = #1 tyNu
                                                val {name=_, ty=t, pos=_} = tydec
                                            in
                                                case t of 
                                                    A.RecordTy _ => ((name, T.RECORD (fn () => makeRec tyNu)) :: fieldlist)
                                                    | A.ArrayTy _ => 
                                                        let
                                                            val arrTy = T.ARRAY (makeArr tyNu)
                                                        in
                                                            tyTable := #1 (Symbol.remove (!tyTable, typ));
                                                            newtenv := Symbol.enter (!newtenv, typ, arrTy);
                                                            ((name, arrTy) :: fieldlist)
                                                        end
                                                    | _ => fieldlist
                                            end
                                            | _ => case ty of
                                                SOME t => ((name, t) :: fieldlist)
                                                | _ => fieldlist
                                    end) [] fields
                                | _ => []
                        in
                            (recordFields, unique, n)
                        end
                    and makeArr (tydec, unique) = 
                        let
                            val {name=n, ty=t, pos=pos} = tydec
                            val elementTy = case t of
                                A.ArrayTy (symbol, p) => (case Symbol.look (!newtenv, symbol) of
                                    SOME ty => SOME ty
                                    | NONE => (
                                        case Symbol.look (!tyTable, symbol) of
                                            SOME tyNu => let
                                                val (tydec, unique) = tyNu
                                                val {name=_, ty=t', pos=_} = tydec
                                            in
                                                case t' of
                                                    A.RecordTy _ => SOME (T.RECORD (fn () => makeRec tyNu))
                                                    | A.ArrayTy _ => 
                                                        let
                                                            val _ = (tyTable := #1 (Symbol.remove (!tyTable, symbol)))
                                                            val arrTy = T.ARRAY (makeArr tyNu)
                                                        in
                                                            newtenv := Symbol.enter (!newtenv, symbol, arrTy);
                                                            SOME arrTy
                                                        end
                                                    | _ => NONE
                                            end
                                            | NONE => (TC.undefinedTypeErr pos symbol; NONE)
                                    ))
                                | _ => NONE
                        in
                            case elementTy of
                                SOME ty => (ty, unique)
                                | NONE => (T.NIL, ref ())
                        end
                in
                    Symbol.appi (fn (name, tyNu) => 
                        let
                            val (tydec, unique) = tyNu
                            val {name=_, ty=t, pos=_} = tydec
                        in
                            case Symbol.look (!tyTable, name) of
                                NONE => ()
                                | SOME _ => (
                                    case t of
                                        A.RecordTy _ => newtenv := Symbol.enter (!newtenv, name, T.RECORD (fn () => makeRec tyNu))
                                        | A.ArrayTy (symbol, _) => 
                                            let 
                                                val arrTy = T.ARRAY (makeArr tyNu)
                                            in
                                                (tyTable := #1 (Symbol.remove (!tyTable, name));
                                                newtenv := Symbol.enter (!newtenv, name, arrTy))
                                            end
                                        | _ => ()
                                )
                        end) (!tyTable);
                    PrintEnv.printEnv (!newvenv, !newtenv);
                    {venv=(!newvenv), tenv=(!newtenv)}
                end
            | A.FunctionDec fundecs => 
                let
                    (*recursively check the signature of each function*)
                    val fundecGroup : (Env.enventry Symbol.table) = Symbol.empty
                    val {venv=newvenv, tenv=newtenv, fundecGroup=_} = foldl (fn (fundec, {venv, tenv, fundecGroup}) => 
                        transFunDec (venv, tenv, fundec, loopDepth, fundecGroup, forbidden, level)
                   ) {venv=venv, tenv=tenv, fundecGroup=fundecGroup} fundecs
                    (*recursively check the body of each function*)
                    val _ = print "begin to check body of each function\n"

                    val _ = map (fn (fundec :Absyn.fundec) => (
                        let 
                        val {name, params, result, body, pos} = fundec
                        val funentry = case Symbol.look (newvenv, name) of
                            NONE => TC.undefinedNameErr pos name
                            | SOME entry => entry
                        (* get the level of function *)
                        val {level=funlevel, ...} = case funentry of
                            Env.FunEntry record => record
                            | _ => (ErrorMsg.error pos ("TypeError: not a function " ^ Symbol.name name); raise ErrorMsg.Error)
                        (* add all args var into a tmp venv to check body*)
                        val (tmpvenv, forbidden) = foldl (fn (param, (venv, forbidden)) => 
                            let
                                (*if param name in forbidden, erase it from forbidden*)
                                val {name, escape, typ, pos} = param
                                val forbidden = case List.find (fn n => n = name) forbidden of
                                    NONE => forbidden
                                    | SOME _ => List.filter (fn n => n <> name) forbidden
                                val ty = case Symbol.look (tenv, typ) of
                                    NONE => TC.undefinedTypeErr pos typ
                                | SOME ty => ty
                            in
                                (* TODO: how to add args into function's level's frame? *)
                                (Symbol.enter (venv, name, Env.VarEntry {access=TL.allocLocal funlevel (!escape), ty=ty}), forbidden)
                            end) (newvenv, forbidden) params
                        val {exp=bodyexp, ty=typ} = transExp (tmpvenv, newtenv, body, 0, forbidden, funlevel)
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
    and transVar (venv:tyvenv , tenv:tytenv, var: A.var, loopDepth: int, forbidden : (Symbol.symbol list), level: TL.level) = let 
            val {exp=_, ty=ty} = case var of
                A.SimpleVar (n, p) => 
                    (case Symbol.look (venv, n) of
                        NONE => TC.undefinedNameErr p n
                        | SOME entry => case entry of
                            Env.VarEntry {access=access, ty=ty} => {exp=TL.NOT_IMPLEMENTED, ty=ty}
                            | _ => (ErrorMsg.error p ("TypeError: not a variable " ^ Symbol.name n); raise ErrorMsg.Error))
                | A.FieldVar (var, n, p) =>
                    let
                        val {exp=_, ty=ty} = transVar (venv, tenv, var, loopDepth, forbidden, level)
                        val genfun = case ty of
                            T.RECORD genfun => genfun
                            | _ => (ErrorMsg.error p ("TypeError: not a record type " ^ T.toString ty); raise ErrorMsg.Error)
                        val (fields, _, _) = genfun ()
                    in
                        case List.find (fn (name, _) => name = n) fields of
                            NONE => TC.undefinedNameErr p n
                            | SOME (_, ty) => {exp=TL.NOT_IMPLEMENTED, ty=ty}
                    end
                | A.SubscriptVar (var, exp, p) => 
                    let
                        val {exp=_, ty=ty} = transVar (venv, tenv, var, loopDepth, forbidden, level)
                        val {exp=_, ty=tyexp} = transExp (venv, tenv, exp, loopDepth, forbidden, level)
                    in
                        TC.checkIsType p (tyexp, T.INT);
                        case ty of 
                            T.ARRAY (ty', _) => {exp=TL.NOT_IMPLEMENTED, ty=ty'}
                            | _ => (ErrorMsg.error p ("TypeError: not an array type " ^ T.toString ty); raise ErrorMsg.Error)
                    end
        in
            {exp=TL.NOT_IMPLEMENTED, ty=ty}
        end
    and transFunDec (venv:tyvenv, tenv:tytenv, fundec:A.fundec, loopDepth: int, fundecGroup: Env.enventry Symbol.table, forbidden : (Symbol.symbol list), level: TL.level) = 
            (* fundec = {name: symbol, params: field list, result: (symbol * pos) option, body: exp pos: pos}*)
            (* recurse through body of fundec, handle recursive fundec*)
            let
                val {name, params, result, body, pos} = fundec
                fun helper (param, (params_ty, params_name, params_escapes)) = 
                        let
                            val {name, escape, typ, pos} = param
                            val _ = case List.find (fn n => n = Symbol.name name) params_name of
                                NONE => ()
                                | SOME _ => (ErrorMsg.error pos ("TypeError: reused parameter name " ^ Symbol.name name); raise ErrorMsg.Error)
                            val ty = case Symbol.look (tenv, typ) of
                                    NONE => TC.undefinedTypeErr pos typ
                                | SOME ty => ty
                        in
                            (ty::params_ty, (Symbol.name name)::params_name, (!escape)::params_escapes)
                        end
                val (params_ty, _, params_escapes) = foldr helper ([], [], []) params
                val res_ty = case result of NONE => T.UNIT
                    | SOME (sym, _) => 
                        case Symbol.look (tenv, sym) of
                            NONE => TC.undefinedTypeErr pos sym
                        | SOME ty => ty
                (*check if function name already exist in fundecGroup*)
                val _ = case Symbol.look (fundecGroup, name) of
                    NONE => ()
                    | SOME _ => (ErrorMsg.error pos ("TypeError: reused function name " ^ Symbol.name name ^ "in same group"); raise ErrorMsg.Error)
                val newLabel = Temp.newlabel()
                val newLevel = TL.newLevel({parent=level, name=newLabel, formals=params_escapes})
                val newVenv = Symbol.enter (venv, name, Env.FunEntry {level=newLevel, label=newLabel, formals=params_ty, result=res_ty}) 
                val newFundecGroup = Symbol.enter (fundecGroup, name, Env.FunEntry {level=newLevel, label=newLabel, formals=params_ty, result=res_ty})
            in
                PrintEnv.printEnv (newVenv, tenv);
                {venv=newVenv, tenv=tenv, fundecGroup=newFundecGroup}
            end
    and transProg (exp: A.exp) = 
        let 
            val venv = Env.base_venv
            val tenv = Env.base_tenv 
            val forbidden : (Symbol.symbol list) = []
            val level = TL.outermost
            val {exp=trexp, ty=_} = transExp (venv, tenv, exp, 0, forbidden, level)
        in
            Printtree.printtree(TextIO.stdOut, TL.unNx trexp);
            (* PrintEnv.printEnv (venv, tenv); *)
            print "\nSemantic Analysis Succeed\n"
        end
end