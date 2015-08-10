(ns maruks.data.splay-heap
  (:refer-clojure :exclude [partition merge]))

(declare insert)
(declare find-min)
(declare delete-min)
(declare heap-seq)
(declare count-nodes)

(deftype TreeNode [left elem right]
  Object
  (hashCode [this]
    (+ (if elem (.hashCode elem) 0)
       (if left (.hashCode left) 0)
       (if right (.hashCode right) 0)))
  (equals [this o]
    (or
     (identical? this o)
     (and (instance? (class this) o)
          (= (.hashCode this) (.hashCode o))))))

(deftype SplayHeap [^TreeNode root cmpfn]
  clojure.lang.IPersistentStack
  (peek [this]
    (find-min root))
  (pop [this]
    (SplayHeap. (delete-min root cmpfn) cmpfn))
  (cons [this e]
    (SplayHeap. (insert cmpfn root e) cmpfn))
  (count [this]
    (count-nodes root))
  (empty [this]
    (SplayHeap. nil cmpfn))
  (equiv [this o]
    (= (seq this) (seq o)))
  clojure.lang.Seqable
  (seq [this]
    (when root
      (heap-seq root cmpfn)))
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
  (if n
    (+ 1 (count-nodes (.left n) (.right n)))
    0))

(defn- to_vec [^TreeNode h]
  [(.left h) (.elem h) (.right h)])

(defn partition [cmpfn ^TreeNode n pivot]
  (letfn [(partition_helper [^TreeNode h]
            (if (nil? h)
              [nil nil]
              (let [[a x b] (to_vec h)]
                (if (cmpfn  x pivot)
                  (if (nil? b)
                    [h nil]
                    (let [[b1 y b2] (to_vec b)]
                      (if (cmpfn y pivot)
                        (let [[small big] (partition_helper b2)]
                          [(->TreeNode (->TreeNode a x b1) y small) big])
                        (let [[small big] (partition_helper b1)]
                          [(->TreeNode a x small) (->TreeNode big y b2)]))))
                  (if (nil? a)
                    [nil h]
                    (let [[a1 y a2] (to_vec a)]
                      (if (cmpfn y pivot)
                        (let [[small big] (partition_helper a2)]
                          [(->TreeNode a1 y small)  (->TreeNode big x b) ])
                        (let [[small big] (partition_helper a1)]
                          [small (->TreeNode big y (->TreeNode a2 x b))]))))))))]
    (partition_helper n)))


(defn insert [cmpfn ^TreeNode h e]
  (let [[a b] (partition cmpfn h e)]
    (->TreeNode a e b)))

(defn find-min [^TreeNode n]
  (when n
    (let [[l e r] (to_vec n)]
      (if (nil? l)
        e
        (recur l)))))

(defn delete-min [^TreeNode n cmpfn]
  (when n
    (let [[l e r] (to_vec n)]
      (if (nil? l)
        r
        (let [[l2 e2 r2] (to_vec l)]
          (if (nil? l2)
            (->TreeNode r2 e r)
            (->TreeNode (delete-min l2 cmpfn) e2 (->TreeNode r2 e r))))))))

(defn heap-seq [^TreeNode n cmpfn]
  (lazy-seq
   (when n
     (cons (find-min n) (heap-seq (delete-min n cmpfn) cmpfn)))))

(defn heap [cmpfn & xs]
  (->SplayHeap (reduce (partial insert cmpfn) nil xs) cmpfn))

(defn min-heap [& xs]
  (apply heap (cons <= xs)))

(defn max-heap [& xs]
  (apply heap (cons >= xs)))
