(ns maruks.data.leftist-heap)

(declare insert)
(declare find-min)
(declare delete-min)
(declare heap-seq)
(declare count-nodes)

(deftype TreeNode [rank elem left right] 
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

(deftype LeftistHeap [^TreeNode root cmpfn]
  clojure.lang.IPersistentStack
  (peek [this]
    (find-min root))
  (pop [this]
    (LeftistHeap. (delete-min root cmpfn) cmpfn))  
  (cons [this e]
    (LeftistHeap. (insert cmpfn root e) cmpfn))
  (count [this]
    (count-nodes root))
  (empty [this]
    (LeftistHeap. nil cmpfn))
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
  (if n (+ 1 (count-nodes (.left n) (.right n))) 0))

(defn rank [^TreeNode h]
  (if (nil? h) 0 (.rank h)))

(defn make-node [e ^TreeNode h1 ^TreeNode h2]
  (if (>= (rank h1) (rank h2))
    (->TreeNode (inc (rank h2)) e h1 h2)
    (->TreeNode (inc (rank h1)) e h2 h1)))

(defn heap-merge [^TreeNode h1 ^TreeNode h2 cmpfn]
  (cond (nil? h1) h2
        (nil? h2) h1
        :else (let [x (.elem h1)
                    a1 (.left h1)
                    b1 (.right h1)
                    y (.elem h2)                    
                    a2 (.left h2)
                    b2 (.right h2)]
                (if (cmpfn  x y) 
                  (make-node x a1 (heap-merge b1 h2 cmpfn))
                  (make-node y a2 (heap-merge h1 b2 cmpfn))))))

(defn insert [cmpfn ^TreeNode h e]
  (heap-merge (->TreeNode 1 e nil nil) h cmpfn))

(defn find-min [^TreeNode n]
  (when n
    (.elem n)))

(defn delete-min [^TreeNode n cmpfn]
  (when n
    (heap-merge (.left n) (.right n) cmpfn)))

(defn heap-seq [^TreeNode n cmpfn]
  (lazy-seq
   (when n
     (cons (find-min n) (heap-seq (delete-min n cmpfn) cmpfn)))))

(defn heap [cmpfn & xs]
  (->LeftistHeap (reduce (partial insert cmpfn) nil xs) cmpfn))

(defn min-heap [& xs]
  (apply heap (cons <= xs)))

(defn max-heap [& xs]
  (apply heap (cons >= xs)))
