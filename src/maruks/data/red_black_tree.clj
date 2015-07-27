(ns maruks.data.red-black-tree)

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

(defn find-node [^TreeNode n k cmpfn]
  (when n
    (let [e (.elem n)
          c (cmpfn e k)]
      (cond
        (pos? c) (find-node (.left n) k cmpfn)
        (neg? c) (find-node (.right n) k cmpfn)
        :else n))))

(defn find-elem [^TreeNode n k cmpfn]
  (let [^TreeNode n (find-node n k cmpfn)]
    (when n
      (.elem n))))

(defn red? [^TreeNode n]
  (and n (= :red (.color n))))

(defn black? [^TreeNode n]
  (and n (= :black (.color n))))

(defn balance [color ^TreeNode left elem ^TreeNode right]
  (let [^TreeNode left-left (and left (.left left))
        ^TreeNode left-right (and left (.right left))
        ^TreeNode right-left (and right (.left right))
        ^TreeNode right-right (and right (.right right))]
    (cond
      (and left left-left (= :black color) (red? left) (red? left-left))
      (->TreeNode :red
                  (->TreeNode :black (.left left-left) (.elem left-left) (.right left-left))
                  (.elem left)
                  (->TreeNode :black (.right left) elem right))

      (and left left-right (= :black color) (red? left) (red? left-right))
      (->TreeNode :red
                  (->TreeNode :black (.left left) (.elem left) (.left left-right))
                  (.elem left-right)
                  (->TreeNode :black (.right left-right) elem right))

      (and right right-left (= :black color) (red? right) (red? right-left))
      (->TreeNode :red
                  (->TreeNode :black left elem (.left right-left))
                  (.elem right-left)
                  (->TreeNode :black (.right right-left) (.elem right) (.right right)))

      (and right right-right (= :black color) (red? right) (red? right-right))
      (->TreeNode :red
                  (->TreeNode :black left elem (.left right))
                  (.elem right)
                  (->TreeNode :black (.left right-right) (.elem right-right) (.right right-right)))

      :else (->TreeNode color left elem right))))

(defn bst-sorted-seq [^TreeNode n]
  (when n (lazy-cat
           (bst-sorted-seq (.left n))
           (list (.elem n))
           (bst-sorted-seq (.right n)))))

(defn count-nodes [^TreeNode n]
  (if n (+ 1 (count-nodes (.left n)) (count-nodes (.right n))) 0))

(declare empty-tree)
(declare insert)
(declare remove-node)

(deftype RedBlackTree [^TreeNode root cmpfn]
  clojure.lang.IPersistentSet
  (disjoin [this k]
    (RedBlackTree. (remove-node root k cmpfn) cmpfn))
  (contains [this k]
    (not (nil? (find-node root k cmpfn))))
  (get [this k]
    (find-elem root k cmpfn))
  (count [this]
    (count-nodes root))
  (empty [this]
    (empty-tree))
  (equiv [this o]
    (= (seq this) (seq o)))
  clojure.lang.IPersistentCollection
  (cons [this e]
    (let [t (insert e root cmpfn)]
      (RedBlackTree. t cmpfn)))
  clojure.lang.Seqable
  (seq [this]
    (when root
      (bst-sorted-seq root)))
  clojure.lang.IFn
  (invoke [this k]
    (find-elem root k cmpfn))
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

(defn insert [e ^TreeNode n cmpfn]
  (letfn [(ins [^TreeNode s]
            (if s
              (let [col (.color s)
                    l (.left s)
                    y (.elem s)
                    r (.right s)
                    c (cmpfn e y)]
                (cond
                 (pos? c) (balance col l y (ins r))
                 (neg? c) (balance col (ins l) y r)
                  :else s))
              (->TreeNode :red nil e nil)))]
    (let [^TreeNode t (ins n)]
      (->TreeNode :black (.left t) (.elem t) (.right t)))))

(defn empty-tree
  ([]
   (empty-tree compare))
  ([cmpfn]
   (->RedBlackTree nil cmpfn)))

(defn red-black-tree
  ([xs]
   (reduce conj (empty-tree) xs))
  ([cmpfn xs]
   (reduce conj (empty-tree cmpfn) xs)))

(defn do-remove-node [^TreeNode n]
  (cond
    (and (red? n)
         (nil? (.left n))
         (nil? (.right n))) nil   ;; read leaf node

         (and (black? n)
              (or (and (red? (.right n)) (nil? (.left n)))
                  (and (red? (.left n)) (nil? (.right n))))) (if-let [left (.left n)]
                                                               (->TreeNode :black nil (.elem left) nil)
                                                               (->TreeNode :black nil (.. n right elem) nil))

    :else (assert false)))

(defn remove-node [^TreeNode n k cmpfn]
  (when n
    (let [e (.elem n)
          l (.left n)
          r (.right n)
          c (.color n)
          d (cmpfn (.elem n) k)]
      (cond
        (pos? d) (->TreeNode c (remove-node l k cmpfn) e r)
        (neg? d) (->TreeNode c l e (remove-node r k cmpfn))
        :else (do-remove-node n)))))
