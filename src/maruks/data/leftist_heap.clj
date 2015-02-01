(ns maruks.data.leftist-heap)


(declare insert)
(declare find-min)
(declare delete-min)
(declare empty-heap)
(declare is-empty?)
(declare heap-seq)

(deftype LeftistHeap [rank elem left right cmpfn]
  clojure.lang.IPersistentStack
  (peek [this]
    (find-min this))
  (pop [this]
    (delete-min this))  
  (cons [this e]
    (insert e this))
  (count [this]
    (if (is-empty? this) 0 (+ 1 (count left) (count right))))
  (empty [this]
    (empty-heap cmpfn))
  (equiv [this o]
    (= (seq this) (seq o)))
  clojure.lang.Seqable
  (seq [this]
    (when-not (is-empty? this)
      (heap-seq this)))
  Object
  (toString [this]
    (clojure.string/join " " (seq this)))
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

(defn is-empty? [^LeftistHeap h]
  (nil? (.elem h)))

(defn rank [^LeftistHeap h]
  (.rank h))

(defn make-node [e ^LeftistHeap h1 ^LeftistHeap h2]
  (if (>= (rank h1) (rank h2))
    (->LeftistHeap (inc (rank h2)) e h1 h2 (.cmpfn h1))
    (->LeftistHeap (inc (rank h1)) e h2 h1 (.cmpfn h1))))

(defn empty-heap [cmpfn]
  (->LeftistHeap 0 nil nil nil cmpfn))

(defn heap-merge [^LeftistHeap h1 ^LeftistHeap h2]
  (cond (is-empty? h1) h2
        (is-empty? h2) h1
        :else (let [x (.elem h1)
                    a1 (.left h1)
                    b1 (.right h1)
                    y (.elem h2)                    
                    a2 (.left h2)
                    b2 (.right h2)]
                (if ((.cmpfn h1) x y) 
                  (make-node x a1 (heap-merge b1 h2))
                  (make-node y a2 (heap-merge h1 b2))))))

(defn insert [e ^LeftistHeap h]
  (let [cmpfn (.cmpfn h)]
    (heap-merge (->LeftistHeap 1 e (empty-heap cmpfn) (empty-heap cmpfn) cmpfn) h)))

(defn find-min [^LeftistHeap h]
  (when-not (is-empty? h)
    (.elem h)))

(defn delete-min [^LeftistHeap h]
  (when-not (is-empty? h)
    (heap-merge (.left h) (.right h))))

(defn heap-seq [^LeftistHeap h]
  (lazy-seq
   (when-not (is-empty? h)
     (cons (find-min h) (heap-seq (delete-min h))))))

(defn heap [cmpfn & xs]
  (reduce conj (empty-heap cmpfn) xs))

(defn min-heap [& xs]
  (reduce conj (empty-heap <=) xs))

(defn max-heap [& xs]
  (reduce conj (empty-heap >=) xs))
