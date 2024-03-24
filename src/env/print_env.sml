functor PrintEnv(Env: ENV) = 
struct
    fun printEnv (venv, tenv) = let
        fun printVenv venv = Symbol.appi (fn (symbol, ventry) => print (Symbol.name symbol ^ ": " ^ Env.enventryToString ventry ^ "\n")) venv
        fun printTenv tenv = Symbol.appi (fn (symbol, ty) => print (Symbol.name symbol ^ ": " ^ Types.toString ty ^ "\n")) tenv
        
        fun venvToString venv = Symbol.foldri (fn (symbol, ventry, acc) => (Symbol.name symbol ^ ": " ^ Env.enventryToString ventry ^ "\n" ^ acc)) "" venv

        fun tenvToString tenv = Symbol.foldri (fn (symbol, ty, acc) => (Symbol.name symbol ^ ": " ^ Types.toString ty ^ "\n" ^ acc)) "" tenv

    in
        (* print "---------new dec---------\n";
        print "---------venv---------\n";
        printVenv venv;
        print "\n---------tenv---------\n";
        printTenv tenv;
        print "\n" *)

        Logger.log Logger.DEBUG ("\n---------new dec---------\n" ^
            "---------venv---------\n" ^
            (venvToString venv) ^
            "\n---------tenv---------\n" ^
            (tenvToString tenv) ^ "\n")
    end
end