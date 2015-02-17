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

(deftype RedBlackTree [^TreeNode root] 
  clojure.lang.IPersistentSet
  (disjoin [this key])
  (contains [this key])
  (get [this key])
  (count [this])
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


(defn empty-tree []
  (->RedBlackTree nil))


(defn red-black-tree-seq [node]
  (cond (nil? node) '()
        :else (cons (.elem node)
                    (concat (red-black-tree-seq (.left node))
                            (red-black-tree-seq (.right node))))))
