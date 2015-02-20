(ns maruks.data.red-black-tree)

(set! *warn-on-reflection* true)

(deftype TreeNode [color left elem right] 
  Object
  (hashCode [this]
    (+ (.hashCode elem)
       (if left (.hashCode left) 0)
       (if right (.hashCode right) 0)))
  (equals [this o]
    (or
     (identical? this o)
     (and (instance? (class this) o)
          (= (.hashCode this) (.hashCode o)))))
  (toString [t]
    (str "< C " (.color t) " L " (.left t) " E " (.elem t) " R " (.right t) " >")))

(declare empty-tree)
(declare count-nodes)
(declare member)
(declare insert)

(deftype RedBlackTree [^TreeNode root] 
  clojure.lang.IPersistentSet
  (disjoin [this k]
    (comment ???))
  (contains [this k]
    (not (nil? (member root k))))
  (get [this k]
    (member root k))
  (count [this]
    (count-nodes root))
  (empty [this]
    (empty-tree))
  (equiv [this o]
    (= (seq this) (seq o)))
  clojure.lang.IPersistentCollection
  (cons [this e]
    (insert e root))  
  clojure.lang.Seqable
  (seq [this]
    (when root
      (tree-seq identity
                (fn [^TreeNode n] (filter identity (list (.left n) (.right n))))
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
  (when n
    (let [e (.elem n)
          c (compare e k)]
      (cond
        (pos? c) (member (.right n) k)
        (neg? c) (member (.left n) k)
        :else e))))

(defn red? [^TreeNode n]
  (and n (= :red (.color n))))

(defn black? [^TreeNode n]
  (and n (= :black (.color n))))

(defn balance [^TreeNode n]
  (cond
    (and (black? (.left n)) (red? (.. n left left)))
    (let [^TreeNode a (.left n)
          ^TreeNode b (.left a)]
      (->TreeNode :red (->TreeNode :black (.left b) (.elem b) (.right b))
                  (.elem a)
                  (->TreeNode :black (.right a) (.elem n) (.right n)) ))

    (and (black? (.left n)) (red? (.. n left right)))
    (let [^TreeNode a (.left n)
          ^TreeNode b (.right a)]
      (->TreeNode :red (->TreeNode :black (.left a) (.elem a) (.left b))
                  (.elem b)
                  (->TreeNode :black  (.right b) (.elem n) (.right n)) ))    

    (and (black? (.right n)) (red? (.. n right left)))
    (let [^TreeNode a (.right n)
          ^TreeNode b (.left a)]
      (->TreeNode :red (->TreeNode :black (.left n) (.elem n) (.left b))
                  (.elem b)
                  (->TreeNode :black (.right b) (.elem n) (.right n)) ))

    (and (black? (.right n)) (red? (.. n right right)))
    (let [^TreeNode a (.right n)
          ^TreeNode b (.right a)]
      (->TreeNode :red (->TreeNode :black (.left n) (.elem n) (.left a))
                  (.elem a)
                  (->TreeNode :black (.left b) (.elem b) (.right b)) ))

    :else n))

(defn insert [e ^TreeNode n]
  (letfn [(ins [^TreeNode s]
            (if s
              (let [col (.color s)
                    l (.left s)
                    y (.elem s)
                    r (.right s)
                    c (compare e y)]
                (cond
                 (pos? c) (balance (->TreeNode col l y (ins r)))
                 (neg? c) (balance (->TreeNode col (ins l) y r))
                  :else s))
              (->TreeNode :red nil e nil)))]
    (let [^TreeNode t (ins n)]
      (->RedBlackTree (->TreeNode :black (.left t) (.elem t) (.right t))))))
