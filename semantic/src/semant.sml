functor Semant (Translate : TRANSLATE) : SEMANT =
struct
    (* alias *)
    structure TL = Translate
    structure TC = TypeChecker
    structure L = Logger
    structure T = Types
    structure Env = Env(Translate)
    structure PrintEnv = PrintEnv(Env)

    type tyvenv = Env.enventry Symbol.table
    type tytenv = T.ty Symbol.table
    type expty = {exp: TL.exp, ty: T.ty}
    type frag = TL.frag
    
    
    
    fun transExp (venv : tyvenv, tenv: tytenv, exp: A.exp, loopDepth: int, level: TL.level, breakLabel: Temp.label option) = 
            case exp of 
                (*To check an IfExp we need to make sure: 1. test is int 2. then and else have same types*)
                A.IfExp {test, then', else', pos} => let
                        val () = L.log L.INFO ("Start to trans IfExp")
                        val {exp=expthen, ty=tythen} = transExp (venv, tenv, then', loopDepth, level, breakLabel)
                        val {exp=exptest, ty=tytest} = transExp (venv, tenv, test, loopDepth, level, breakLabel)
                    in 
                        case else' of SOME elseexp => let 
                                    val {exp=expelse, ty=tyelse} = transExp (venv, tenv, elseexp, loopDepth, level, breakLabel)
                                in
                            
                                    TC.checkIfExp pos (tytest, tythen, tyelse);
                                    {exp=TL.transIf(exptest, expthen, SOME(expelse)), ty=tythen}
                                end
                        | NONE => let in
                                TC.checkIsType pos (tytest, T.INT);
                                TC.checkIsType pos (tythen, T.UNIT);
                                {exp=TL.transIf(exptest, expthen, NONE), ty=tythen}
                            end
                    end
            | A.OpExp {left, oper, right, pos} =>
                let
                    val () = L.log L.INFO ("Start to trans OpExp")
                    val {exp=expleft, ty=tyleft} = transExp (venv, tenv, left, loopDepth, level, breakLabel)
                    val {exp=expright, ty=tyright} = transExp (venv, tenv, right, loopDepth, level, breakLabel)
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
                    val () = L.log L.INFO ("Start to trans LetExp")
                    val {venv=venv', tenv=tenv', explist=explist'} = foldl (fn (dec, {venv, tenv, explist}) => 
                                let
                                    val _ = L.log L.DEBUG "begin to check dec"
                                    val {venv=venv, tenv=tenv, exp=exp} = transDec (venv, tenv, dec, loopDepth, level, breakLabel)
                                in
                                    case exp of 
                                        SOME exp => {venv=venv, tenv=tenv, explist=explist@[exp]}
                                    | NONE => {venv=venv, tenv=tenv, explist=explist}
                                end) {venv=venv, tenv=tenv, explist=[]} decs
                    val {exp=expbody, ty=tybody} = transExp (venv', tenv', body, loopDepth, level, breakLabel)
                in
                    L.log L.DEBUG "begin to check body";
                    {exp=TL.transLet(explist', expbody), ty=tybody}
                end
            | A.IntExp intval => (L.log L.INFO ("Start to trans int: " ^ Int.toString(intval)); {exp=TL.transInt intval, ty=T.INT})
            | A.StringExp (str, _) => (L.log L.INFO ("Start to trans String: " ^ str); {exp=TL.transString str, ty=T.STRING})
            | A.NilExp => (L.log L.INFO ("Start to trans nil"); {exp=TL.transNil(), ty=T.NIL})
            | A.SeqExp exps => 
                let
                    val () = L.log L.INFO "Start to trans SeqExp"
                    fun help ((exp, _), (explist, ty)) = let
                                val {exp=entryExp, ty=entryTy} = transExp (venv, tenv, exp, loopDepth, level, breakLabel)
                            in
                                (entryExp::explist, entryTy)
                            end
                    val (explist, lastTy) = foldl help ([], T.UNIT) exps
                in
                    L.log L.DEBUG "SeqExp done";
                    {exp=TL.transSeq(explist), ty=lastTy}
                end
            | A.VarExp var => let 
                    val ans = transVar (venv, tenv, var, loopDepth, level, breakLabel)
                    val _ = L.log L.DEBUG "VarExp Done"
                in
                    ans
                end
            | A.AssignExp {var : A.var, exp: A.exp, pos :A.pos} => let
                    (*if var name in forbidden, raise error*)
                    val () = L.log L.INFO "Start to trans AssignExp"
                    val {exp=varexp, ty=tyvar} = transVar (venv, tenv, var, loopDepth, level, breakLabel)
                    val {exp=expexp, ty=tyexp} = transExp (venv, tenv, exp, loopDepth, level, breakLabel)
                in
                    TC.checkSameType pos (tyvar, tyexp);
                    {exp=TL.transAssign(varexp, expexp), ty=T.UNIT}
                end
            | A.WhileExp {test, body, pos} => let
                    val () = L.log L.INFO "Start to trans WhileExp"

                    val {exp=exptest, ty=tytest} = transExp (venv, tenv, test, loopDepth, level, breakLabel)
                    val breakPoint = Temp.newlabel()
                    val {exp=expbody, ty=tybody} = transExp (venv, tenv, body, loopDepth+1, level, SOME breakPoint)
                in
                    TC.checkIsType pos (tytest, T.INT);
                    TC.checkIsType pos (tybody, T.UNIT);
                    {exp=TL.transWhile(exptest, expbody, breakPoint), ty=T.UNIT}
                end
            | A.ForExp {var, escape, lo, hi, body, pos} => let
                    val () = L.log L.INFO "Start to trans ForExp"
                    val {exp=explo, ty=tylo} = transExp (venv, tenv, lo, loopDepth, level, breakLabel)
                    val {exp=exphi, ty=tyhi} = transExp (venv, tenv, hi, loopDepth, level, breakLabel)
                    
                    val access = TL.allocLocal level (!escape)
                    val newVenv = Symbol.enter (venv, var, Env.VarEntry {access=access, ty=T.INT})
                    val () = L.log L.DEBUG "begin to check body"
                    val breakPoint = Temp.newlabel()
                    val {exp=expbody, ty=tybody} = transExp (newVenv, tenv, body, loopDepth+1, level, SOME breakPoint)
                    val () = L.log L.DEBUG "body done"
                in          
                    PrintEnv.printEnv (newVenv, tenv);
                    TC.checkIsType pos (tylo, T.INT);
                    TC.checkIsType pos (tyhi, T.INT);
                    TC.checkIsType pos (tybody, T.UNIT);
                    {exp=TL.transFor(TL.simpleVar(access, level), explo, exphi, expbody, breakPoint), ty=T.UNIT}
                end
            | A.BreakExp pos => if loopDepth > 0 then {exp=TL.transBreak(breakLabel), ty=T.UNIT}
                else (ErrorMsg.error pos "SyntaxError: break outside loop"; raise ErrorMsg.Error)
            (*0. check if func exist 1. check if args align with definition *)
            | A.CallExp {func, args, pos} => let
                    val () = L.log L.INFO ("Start to trans CallExp: " ^ Symbol.name func)
                    val {level=funclevel, label, formals, result} = case Symbol.look (venv, func) of
                            SOME(Env.FunEntry record) => record
                        | _ => (ErrorMsg.error pos ("NameError: function not find" ^ Symbol.name func); raise ErrorMsg.Error)
                    (*check if the number of args align with the number of formals*)
                    (* val () = L.log L.DEBUG ("level of func: " ^ (case funclevel of ROOT => "ROOT" | _ => "not ROOT")) *)
                    val () = L.log L.DEBUG ("length of formals: " ^ Int.toString (length formals))
                    val _ = if (length args) = (length formals) then ()
                        else (ErrorMsg.error pos ("TypeError: " ^ Symbol.name func ^ " () takes " ^ Int.toString (length formals) ^ " positional argument(s) but called with" ^ Int.toString (length args) ^ " argument(s)"); raise ErrorMsg.Error)
                    val expargs = foldr (fn ((arg, formal), expargs) =>
                                let
                                    val {exp=exparg, ty=tyarg} = transExp (venv, tenv, arg, loopDepth, level, breakLabel)
                                in
                                    TC.checkSameType pos (tyarg, formal);
                                    exparg::expargs
                                end) [] (ListPair.zip(args, formals))
                in
                    {exp=TL.transCall(label, funclevel, level, expargs), ty=result}
                end
            (*0. check if the type of record exist in tenv 1. check if the field name&type aigned with definition*)
            | A.RecordExp {fields, typ, pos} => let
                    val () = L.log L.INFO "Start to trans RecordExp"
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
                                    val {exp=_, ty=tyexp} = transExp (venv, tenv, exp, loopDepth, level, breakLabel)
                                    (*check if the field name can be found in definition*)
                            
                                in
                                    case List.find (fn (name, _) => name = symbol) recordFields of
                                        NONE => TC.undefinedNameErr pos symbol
                                    | SOME (name, ty) => 
                                        TC.checkSameType pos (tyexp, ty)
                                end) fields
                    (* check if all recordFields can be find in field*)
                    val expfields = foldr (fn ((name, typ), expfields) =>
                                case List.find (fn (symbol, exp, pos) => symbol = name) fields of
                                    NONE => TC.undefinedNameErr pos name
                                | SOME (symbol, exp, pos) => let
                                        val {exp=expfield, ty=tyexp} = transExp (venv, tenv, exp, loopDepth, level, breakLabel)
                                    in
                                        TC.checkSameType pos (tyexp, typ);
                                        expfield::expfields
                                    end) [] recordFields
                in
                    {exp=TL.transRecord(expfields), ty=ty}
                end
            
            
            | A.ArrayExp {typ, size, init, pos} =>  (*TODO*)
                (*0. check if the type of array exist in tenv 1. check if the type of init is same as the type of array*)
                let 
                    val () = L.log L.INFO "Start trans ArrayExp"
                    val ty = case Symbol.look (tenv, typ) of
                            NONE => TC.undefinedTypeErr pos typ
                        | SOME ty => ty
                    val {exp=expsize, ty=tysize} = transExp (venv, tenv, size, loopDepth, level, breakLabel)
                    val {exp=expinit, ty=tyinit} = transExp (venv, tenv, init, loopDepth, level, breakLabel)
                in  
                    TC.checkIsType pos (tysize, T.INT);
                    (case ty of T.ARRAY (ty',_) => TC.checkSameType pos (ty', tyinit)
                        | _ => (ErrorMsg.error pos ("TypeError: not an array type " ^ Symbol.name typ); raise ErrorMsg.Error));
                
                    {exp=TL.transArray(expsize, expinit), ty=ty}
                end

    and transDec (venv : tyvenv, tenv: tytenv, dec : A.dec, loopDepth: int, level: TL.level, breakLabel : Temp.label option) = 
            (*if declared variable has same name with forbidden, erase it from forbidden*)
            case dec of A.VarDec{name, escape, typ, init, pos}=> let 
                        val _ = L.log L.INFO ("transVarDec: " ^ Symbol.name name ^ ", escape: "^ Bool.toString (!escape) ^ ", typ: " ^ (case typ of NONE => "NONE" | SOME t => Symbol.name (#1 t)));
                        val {exp=exp', ty=tyinit} = transExp (venv, tenv, init, loopDepth, level, breakLabel)
                        val newtyp = case typ of NONE => tyinit
                            | SOME t => 
                                case Symbol.look (tenv, #1 t) of SOME ty => ty
                                | NONE => TC.undefinedTypeErr pos (#1 t)           
                    in
                        if T.equals(newtyp, tyinit) then 
                            let 
                                val access = TL.allocLocal level (!escape)
                                val newVenv = Symbol.enter(venv, name, Env.VarEntry {access=access, ty=newtyp})
                                val _ = PrintEnv.printEnv (newVenv,tenv)
                            in
                                {venv = newVenv, tenv = tenv, exp=SOME(TL.transVarDec(access, exp'))}
                            end
                        else 
                            case newtyp of T.NIL => 
                                    (ErrorMsg.error pos ("SyntaxError: using nil to initialize variable without type"); raise ErrorMsg.Error)
                            | _ => 
                                (ErrorMsg.error pos ("TypeError: unmatched type: var " ^ Symbol.name name ^ " is " ^ T.toString tyinit ^ ", but declared as " ^ T.toString newtyp); raise ErrorMsg.Error)
                    end
            | A.TypeDec tydecs => let 
                    val duplicatedChecker = map TC.checkRecordName tydecs (*for all A.recordTy in tydecs, check*)
                    val reuseChecker = foldl TC.checkReuse Symbol.empty tydecs
                    val newvenv : (Env.enventry Symbol.table) ref = ref venv
                    val newtenv : (T.ty Symbol.table) ref = ref tenv
                    val aliasTable : ((unit Symbol.table) Symbol.table) ref = ref Symbol.empty
                    val tyTable : ((A.tydec * T.unique) Symbol.table) ref = ref Symbol.empty
                    fun updateAliasHelper (name, ty) = 
                            case Symbol.look (!tyTable, name) of NONE => ()
                            | SOME _ => (
                                    newtenv := Symbol.enter (!newtenv, name, ty);
                                    tyTable := #1 (Symbol.remove (!tyTable, name));
                                    case Symbol.look (!aliasTable, name) of NONE => ()
                                    |    SOME alias => Symbol.appi (fn (n, _) => updateAliasHelper (n, ty)) alias
                                )
                    
                    and updateAliasRecAndArr (name, tyNu) = (
                                tyTable := Symbol.enter (!tyTable, name, tyNu);
                                case Symbol.look (!aliasTable, name) of NONE => ()
                                | SOME alias => Symbol.appi (fn (n, _) => updateAliasRecAndArr (n, tyNu)) alias
                            )
                    and updateAliasName (name, n) = 
                            case Symbol.look (!newtenv, n) of NONE => (
                                        case Symbol.look (!tyTable, n) of NONE => 
                                                aliasTable := Symbol.enter (!aliasTable, n, Symbol.enter (
                                                        case Symbol.look (!aliasTable, n) of SOME aT => aT
                                                        | NONE => Symbol.empty, name, ()
                                                        )
                                                    )
                                        | SOME ({name=n, ty=t, pos=p}, unique) => case t of 
                                                A.NameTy _ => TC.undefinedTypeErr p n (* type a = b, type b = a*)
                                            | _ => updateAliasRecAndArr (name, ({name=n, ty=t, pos=p}, unique))
                                    )
                            | SOME ty => updateAliasHelper (name, ty)
  
                    and updateTydecAlias(tydec) = 
                            let
                                val {name, ty, ...} = tydec
                            in
                                case ty of A.NameTy (n, _) => (
                                            tyTable := Symbol.enter (!tyTable, name, (tydec, ref ())); 
                                            updateAliasName (name, n)
                                        )
                                | _ => updateAliasRecAndArr (name, (tydec, ref ()))
                            end

                    fun makeRec (tyNu) = 
                            let
                                val ({name=n, ty=t, pos=pos}, unique) = tyNu
                                val recordFields = case t of
                                        A.RecordTy fields => 
                                            foldr (fn (field, fieldlist) =>
                                                    let
                                                        val {name, escape, typ, pos} = field
                                                        val tyNu = Symbol.look (!tyTable, typ)     
                                                        val ty = case tyNu of NONE => (
                                                                        case Symbol.look (!newtenv, typ) of NONE => 
                                                                                TC.undefinedTypeErr pos typ
                                                                        | SOME ty => SOME ty
                                                                    )
                                                            | SOME tyNu => NONE  
                                                    in
                                                        case tyNu of SOME tyNu => 
                                                                let
                                                                    val ({ty=t, ...}, _) = tyNu
                                                                in
                                                                    case t of A.RecordTy _ => 
                                                                            (name, T.RECORD (fn () => makeRec tyNu)) :: fieldlist
                                                                    | A.ArrayTy _ => 
                                                                        let
                                                                            val arrTy = T.ARRAY (makeArr tyNu)
                                                                        in
                                                                            tyTable := #1 (Symbol.remove (!tyTable, typ));
                                                                            newtenv := Symbol.enter (!newtenv, typ, arrTy);
                                                                            (name, arrTy) :: fieldlist
                                                                        end
                                                                    | A.NameTy _ => fieldlist
                                                                end
                                                        | NONE => case ty of SOME t => (name, t) :: fieldlist
                                                            | NONE => fieldlist
                                                    end) [] fields
                                    | _ => []
                            in
                                
                                (recordFields, unique, n)
                            end
                    and makeArr (tyNu) = 
                            let
                                val ({name=n, ty=t, pos=pos}, unique) = tyNu
                                val elementTy = case t of A.ArrayTy (symbol, _) => (
                                                case Symbol.look (!newtenv, symbol) of SOME ty => SOME ty
                                                | NONE => (
                                                        case Symbol.look (!tyTable, symbol) of SOME tyNu => 
                                                                let
                                                                    val ({ty=t',...}, _) = tyNu
                                                                in
                                                                    case t' of A.RecordTy _ => 
                                                                            SOME (T.RECORD (fn () => makeRec tyNu))
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
                                                        | NONE => TC.undefinedTypeErr pos symbol
                                                    )
                                            )
                                    | _ => NONE
                            in
                                case elementTy of SOME ty => (ty, unique)
                                | NONE => (T.NIL, ref ())
                            end
                    fun help(name, tyNu) = 
                            let
                                val ({ty=t, ...}, _) = tyNu (* tyNu: tydec * ref () *)
                            in
                                case Symbol.look (!tyTable, name) of NONE => ()
                                | SOME _ => (
                                        case t of  A.NameTy _ => ()
                                        | A.RecordTy _ => 
                                            newtenv := Symbol.enter (!newtenv, name, T.RECORD (fn () => makeRec tyNu))
                                        | A.ArrayTy _ => (
                                                tyTable := #1 (Symbol.remove (!tyTable, name));
                                                newtenv := Symbol.enter (!newtenv, name, T.ARRAY (makeArr tyNu))
                                            )
                                    )
                            end
                in
                    map updateTydecAlias tydecs; 
                    Symbol.appi help (!tyTable);
                    PrintEnv.printEnv (!newvenv, !newtenv);
                    {venv=(!newvenv), tenv=(!newtenv), exp=NONE}
                end
            | A.FunctionDec fundecs => (*recursively check the signature of each function*)
                let 
                    val fundecGroup : (Env.enventry Symbol.table) = Symbol.empty
                    val {venv=newvenv, tenv=newtenv, ...} = foldl (fn (fundec, {venv, tenv, fundecGroup}) => 
                                transFunDec (venv, tenv, fundec, loopDepth, fundecGroup, level)
                        ) {venv=venv, tenv=tenv, fundecGroup=fundecGroup} fundecs
                    (*recursively check the body of each function*)
                    val _ = L.log L.DEBUG "begin to check body of each function"
                    fun paramTmpVenv (params, newvenv, level) = 
                            let fun helper (param, venv) = 
                                        let
                                            val {name, escape, typ, pos} = param
                                            val ty = case Symbol.look (tenv, typ) of SOME ty => ty
                                                | NONE => TC.undefinedTypeErr pos typ     
                                        in
                                            (* TODO: how to add args into function's level's frame? *)
                                            (Symbol.enter (venv, name, Env.VarEntry {access=TL.allocLocal level (!escape), ty=ty}))
                                        end         
                            in foldl helper newvenv params end

                    val expfuncs = foldr (fn (fundec :Absyn.fundec, expfuncs) => (
                                    let 
                                        val {name, params, result, body, pos} = fundec
                                        (* get the def level of function *)
                                        val funentry = case Symbol.look (newvenv, name) of
                                                NONE => TC.undefinedNameErr pos name
                                            | SOME entry => entry
                                        val {level=fundeclevel, ...} = case funentry of Env.FunEntry record => record
                                        | _ => (ErrorMsg.error pos ("TypeError: not a function " ^ Symbol.name name); raise ErrorMsg.Error)
                                        (* add all args var into a tmp venv to check body*)
                                        val bodylevel = TL.newLevel({parent=fundeclevel, name=Temp.namedlabel "function", formals=[]})
                                        val tmpvenv = paramTmpVenv (params, newvenv, bodylevel)
                                        val {exp=bodyexp, ty=typ} = transExp (tmpvenv, newtenv, body, 0, bodylevel, breakLabel)
                                        (*check if expty consistent with the delared function ty*)
                                        val () = L.log L.DEBUG ("func body typ: " ^ T.toString typ)
                                    in
                                        TC.checkFunc(newtenv, result, typ, pos);
                                        TL.transFunDec(fundeclevel, bodyexp)::expfuncs
                                    end)) [] fundecs
                in
                    PrintEnv.printEnv (newvenv, newtenv);
                    {venv=newvenv, tenv=newtenv, exp=SOME(TL.transFunDecs(expfuncs))}
                end
    and transVar (venv:tyvenv , tenv:tytenv, var: A.var, loopDepth: int,  level: TL.level, breakLabel: Temp.label option) = let 
                val {exp=exp', ty=ty} = case var of
                        A.SimpleVar (n, p) => 
                            (case Symbol.look (venv, n) of
                                    NONE => TC.undefinedNameErr p n
                                | SOME entry => case entry of
                                        Env.VarEntry {access=access, ty=ty} => (L.log L.DEBUG "start trans simple var"; {exp=TL.simpleVar(access, level), ty=ty})
                                    | _ => (ErrorMsg.error p ("TypeError: not a variable " ^ Symbol.name n); raise ErrorMsg.Error))
                    | A.FieldVar (var, n, p) =>
                        let
                            val {exp=varExp, ty=ty} = transVar (venv, tenv, var, loopDepth, level, breakLabel)
                            val (fields, _, _) = case ty of T.RECORD genfun => genfun ()

                                | _ => (ErrorMsg.error p ("TypeError: not a record type " ^ T.toString ty); raise ErrorMsg.Error)
                        in
                            case List.findi (fn (i,(name, _)) => name = n) fields of NONE => TC.undefinedNameErr p n 
                            | SOME (idx, (_, ty)) => {exp=TL.fieldVar(varExp, idx), ty=ty}
                        end
                    | A.SubscriptVar (var, exp, p) => 
                        let
                            val {exp=arrExp, ty=ty} = transVar (venv, tenv, var, loopDepth, level, breakLabel)
                            val {exp=idxExp, ty=tyexp} = transExp (venv, tenv, exp, loopDepth, level, breakLabel)
                        in
                            TC.checkIsType p (tyexp, T.INT);
                            case ty of 
                                T.ARRAY (ty', _) => {exp=TL.subscriptVar(arrExp,idxExp), ty=ty'}
                            | _ => (ErrorMsg.error p ("TypeError: not an array type " ^ T.toString ty); raise ErrorMsg.Error)
                        end
            in
                {exp=exp', ty=ty}
            end
    and transFunDec (venv:tyvenv, tenv:tytenv, fundec:A.fundec, loopDepth: int, fundecGroup: Env.enventry Symbol.table, level: TL.level) = 
            (* fundec = {name: symbol, params: field list, result: (symbol * pos) option, body: exp pos: pos}*)
            (* recurse through body of fundec, handle recursive fundec*)
            let
                val {name, params, result, body, pos} = fundec
                val () = L.log L.INFO ("transFunDec: " ^ Symbol.name name)
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
                val newVenv = Symbol.enter (venv, name, Env.FunEntry {level=level, label=newLabel, formals=params_ty, result=res_ty}) 
                val newFundecGroup = Symbol.enter (fundecGroup, name, Env.FunEntry {level=level, label=newLabel, formals=params_ty, result=res_ty})
                (* val newVenv = Symbol.enter (venv, name, Env.FunEntry {level=level, label=newLabel, formals=params_ty, result=res_ty}) 
                val newFundecGroup = Symbol.enter (fundecGroup, name, Env.FunEntry {level=level, label=newLabel, formals=params_ty, result=res_ty}) *)
                (* TODO: why newLevel cause error? *)
            in
                PrintEnv.printEnv (newVenv, tenv);
                {venv=newVenv, tenv=tenv, fundecGroup=newFundecGroup}
            end
    and transProg (exp: A.exp) = 
            let 
                val () = L.log L.INFO "Start to translate program"
                val venv = Env.base_venv
                val tenv = Env.base_tenv 
                val startLevel = TL.newLevel({parent = TL.outermost, name = Temp.namedlabel "main", formals = []})
                val {exp=trexp, ty=_} = transExp (venv, tenv, exp, 0,  startLevel, NONE)
            in
                (* Printtree.printtree(TextIO.stdOut, TL.unNx trexp);
                (* PrintEnv.printEnv (venv, tenv); *)
                print "\nSemantic Analysis Succeed\n"; *)
                TL.getResult()
            end
end