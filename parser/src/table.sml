(* functor IntMapTable (type key
		     val getInt: key -> int) : TABLE =
struct
  type key=key
  type 'a table = 'a IntBinaryMap.map
  val empty = IntBinaryMap.empty
  fun enter(t,k,a) = IntBinaryMap.insert(t,getInt k,a)
  fun look(t,k) = IntBinaryMap.find(t,getInt k)
  fun app f t = IntBinaryMap.app f t
end *)

functor RedBlackMapTable (type key
         val getInt: key -> int) : TABLE =
struct
  structure RedBlackMap = RedBlackMapFn(type ord_key = key 
    val compare = fn (k1, k2) => Int.compare(getInt k1, getInt k2))
  type key=key
  type 'a table = 'a RedBlackMap.map
  val empty = RedBlackMap.empty
  fun enter(t, k, a) = RedBlackMap.insert(t, k, a)
  fun look(t, k) = RedBlackMap.find(t, k)
  fun appi f t = RedBlackMap.appi f t
end