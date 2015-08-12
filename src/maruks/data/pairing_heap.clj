(ns maruks.data.pairing-heap
  (:refer-clojure :exclude [merge]))

(declare insert)
(declare find-min)
(declare delete-min)
(declare heap-seq)
(declare count-nodes)

(deftype TreeNode [elem children]
  Object
  (hashCode [this]
    (+ (if elem (.hashCode elem) 0)
       (if children (.hashCode children) 0)))
  (equals [this o]
    (or
     (identical? this o)
     (and (instance? (class this) o)
          (= (.hashCode this) (.hashCode o))))))

(deftype PairingHeap [^TreeNode root cmpfn]
  clojure.lang.IPersistentStack
  (peek [this]
    (find-min this))
  (pop [this]
    (PairingHeap. (delete-min root cmpfn) cmpfn))
  (cons [this e]
    (PairingHeap. (insert cmpfn root e) cmpfn))
  (count [this]
    (count-nodes root))
  (empty [this]
    (PairingHeap. nil cmpfn))
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
    (inc (reduce + (map count-nodes (.children n))))
    0))

(defn heap-seq [^TreeNode n cmpfn]
  (lazy-seq
   (when n
     (cons (.elem n) (heap-seq (delete-min n cmpfn) cmpfn)))))

(defn- do-merge [^TreeNode h1 ^TreeNode h2 cmpfn]
  (let [x   (.elem h1)
        hs1 (.children h1)
        y   (.elem h2)
        hs2 (.children h2)]
    (if (cmpfn x y)
      (->TreeNode x (cons h2 hs1))
      (->TreeNode y (cons h1 hs2)))))

(defn merge [^TreeNode h1 ^TreeNode h2 cmpfn]
  (cond (nil? h1) h2
        (nil? h2) h1
        :else (do-merge h1 h2 cmpfn)))

(defn insert [cmpfn ^TreeNode n e]
  (merge (->TreeNode e '()) n cmpfn))

(defn merge-pairs [ps cmpfn]
  (cond (empty? ps) nil
        (= 1 (count ps)) (first ps)
        :else (let [[h1 & [h2 & hs]] ps]
                (merge (merge h1 h2 cmpfn) (merge-pairs hs cmpfn) cmpfn))))

(defn find-min [^PairingHeap heap]
  (when (.root heap)
    (.elem (.root heap))))

(defn delete-min [^TreeNode node cmpfn]
  (when node
    (merge-pairs (.children node) cmpfn)))

(defn heap [cmpfn & xs]
  (->PairingHeap (reduce (partial insert cmpfn) nil xs) cmpfn))

(defn min-heap [& xs]
  (apply heap (cons <= xs)))

(defn max-heap [& xs]
  (apply heap (cons >= xs)))
