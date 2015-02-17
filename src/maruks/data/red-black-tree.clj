(ns maruks.data.red-black-tree)

(deftype TreeNode [color elem left right] 
  Object
  (hashCode [this]
    (+ (.hashCode elem)
       (if left (.hashCode left) 0)
       (if right (.hashCode right) 0)))
  (equals [this o]
    (or
     (identical? this o)
     (and (instance? (class this) o)
          (= (.hashCode this) (.hashCode o))))))

(declare empty-tree)
(declare count-nodes)
(declare member?)
(declare member)

(deftype RedBlackTree [^TreeNode root] 
  clojure.lang.IPersistentSet
  (disjoin [this k])
  (contains [this k]
    (member? root k))
  (get [this k]
    (member root k))
  (count [this]
    (count-nodes root))
  (empty [this]
    (empty-tree))
  (equiv [this o]
    (= (seq this) (seq o)))
  clojure.lang.IPersistentCollection
  (cons [this o] this)  
  clojure.lang.Seqable
  (seq [this]
    (when root
      (tree-seq :color
                (fn [^TreeNode n] (filter identity [(.left n) (.right n)]))
                root)))
  Object
  (toString [this]
    (clojure.string/join " " (seq this)))
  (hashCode [this]
    (.hashCode root))
  (equals [this o]
    (or
     (identical? this o)
     (and (instance? (class this) o)
          (= (.hashCode root) (.hashCode o))))))

(defn count-nodes [^TreeNode n]
  (if n (+ 1 (count-nodes (.left n) (.right n))) 0))

(defn empty-tree []
  (->RedBlackTree nil))

(defn member [^TreeNode n k]



  )
