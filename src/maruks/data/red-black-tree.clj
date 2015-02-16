(ns maruks.data.red-black-tree)


(deftype TreeNode [color elem left right] 

  clojure.lang.IPersistentSet

  (disjoin [this key])
  (contains [this key])
  (get [this key])
  (count [this])
  (empty [this])
  (equiv [this o])

  clojure.lang.Seqable
  (seq [this])  
  
  Object
  (toString [this]
    (clojure.string/join " " (seq this)))
  (hashCode [this]
    (.hashCode elem))
  (equals [this o]
    (or
     (identical? this o)
     (and (instance? (class this) o)
          (= (.hashCode elem) (.hashCode o))))))
