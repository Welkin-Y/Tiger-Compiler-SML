signature ENV = 
sig
    type access
    type level 
    
    datatype enventry = VarEntry of {access: access, ty: Types.ty}
                        | FunEntry of {level: level, label: Temp.label, formals: Types.ty list, result: Types.ty}
    val base_tenv : Types.ty Symbol.table
    val base_venv : enventry Symbol.table
    val enventryToString : enventry -> string
end