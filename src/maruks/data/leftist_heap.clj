(ns maruks.data.leftist-heap)


(set! *warn-on-reflection* true)

(declare insert)
(declare find-min)
(declare delete-min)
(declare empty-node)
(declare is-empty?)
(declare heap-seq)
(declare count-nodes)

(deftype TreeNode [rank elem left right] 
  Object
  (hashCode [this]
    (+ (.hashCode rank)
       (if (nil? elem) 1 (.hashCode elem))              
       (if (nil? left) 2 (.hashCode left))
       (if (nil? right) 3 (.hashCode right))))
  (equals [this o]
    (or
     (identical? this o)
     (and (instance? (class this) o)
          (= (.hashCode this) (.hashCode o))))))

(deftype LeftistHeap [^TreeNode root cmpfn]
  clojure.lang.IPersistentStack
  (peek [this]
    (find-min root))
  (pop [this]
    (LeftistHeap. (delete-min root cmpfn) cmpfn))  
  (cons [this e]
    (LeftistHeap. (insert root e cmpfn) cmpfn))
  (count [this]
    (count-nodes root))
  (empty [this]
    (LeftistHeap. (empty-node) cmpfn))
  (equiv [this o]
    (= (seq this) (seq o)))
  clojure.lang.Seqable
  (seq [this]
    (when-not (is-empty? root)
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
  (if (is-empty? n) 0 (+ 1 (count-nodes (.left n) (.right n)))))

(defn is-empty? [^TreeNode h]
  (nil? (.elem h)))

(defn rank [^TreeNode h]
  (.rank h))

(defn make-node [e ^TreeNode h1 ^TreeNode h2]
  (if (>= (rank h1) (rank h2))
    (->TreeNode (inc (rank h2)) e h1 h2)
    (->TreeNode (inc (rank h1)) e h2 h1)))

(defn empty-node []
  (->TreeNode 0 nil nil nil))

(defn heap-merge [^TreeNode h1 ^TreeNode h2 cmpfn]
  (cond (is-empty? h1) h2
        (is-empty? h2) h1
        :else (let [x (.elem h1)
                    a1 (.left h1)
                    b1 (.right h1)
                    y (.elem h2)                    
                    a2 (.left h2)
                    b2 (.right h2)]
                (if (cmpfn  x y) 
                  (make-node x a1 (heap-merge b1 h2 cmpfn))
                  (make-node y a2 (heap-merge h1 b2 cmpfn))))))

(defn insert [^TreeNode h e cmpfn]
  (heap-merge (->TreeNode 1 e (empty-node) (empty-node)) h cmpfn))

(defn find-min [^TreeNode h]
  (when-not (is-empty? h)
    (.elem h)))

(defn delete-min [^TreeNode h cmpfn]
  (when-not (is-empty? h)
    (heap-merge (.left h) (.right h) cmpfn)))

(defn heap-seq [^TreeNode h cmpfn]
  (lazy-seq
   (when-not (is-empty? h)
     (cons (find-min h) (heap-seq (delete-min h cmpfn) cmpfn)))))

(defn heap [cmpfn & xs]
  (->LeftistHeap (reduce #(insert %1 %2 cmpfn) (empty-node) xs) cmpfn))

(defn min-heap [& xs]
  (apply heap (cons <= xs)))

(defn max-heap [& xs]
  (apply heap (cons >= xs)))
