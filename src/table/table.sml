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
  fun remove(t, k) = RedBlackMap.remove(t, k)
  fun foldri f b t = RedBlackMap.foldri f b t
end