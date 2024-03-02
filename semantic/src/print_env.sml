structure PrintEnv = 
struct
    fun printEnv (venv, tenv) = let
        fun printVenv venv = Symbol.appi (fn (symbol, ventry) => print (Symbol.name symbol ^ ": " ^ Env.enventryToString ventry ^ "\n")) venv
        fun printTenv tenv = Symbol.appi (fn (symbol, ty) => print (Symbol.name symbol ^ ": " ^ Types.toString ty ^ "\n")) tenv
    in
        print "---------new dec---------\n";
        print "---------venv---------\n";
        printVenv venv;
        print "\n---------tenv---------\n";
        printTenv tenv;
        print "\n"
    end
end