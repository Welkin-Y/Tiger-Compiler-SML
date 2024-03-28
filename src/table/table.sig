signature TABLE = 
sig
   type key
   type 'a table
   val empty : 'a table
   val enter : 'a table * key * 'a -> 'a table
   val look  : 'a table * key -> 'a option
   val appi : ((key * 'a) -> unit) -> 'a table -> unit
   val remove : 'a table * key -> 'a table * 'a
   val foldri : (key * 'a * 'b -> 'b) -> 'b -> 'a table -> 'b
end

