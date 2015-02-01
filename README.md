# maruks.data.core

Persistent data structures based on "Purely Functional Data Structures" book.

## Leftist heap

Leftist heap implements persistent stack operations:

 operation    |                                 |  time complexity  |
------------- | --------------------------------|-------------------|
peek          | returns the smallest element    |   O(1)            |
pop           | removes the smallest element    |   O(log n)        |
conj          | adds element to the heap        |   O(log n)        |
count         | returns heap's size             |   O(n)            |
empty         | returns empty heap              |   O(1)            |
seq           | returns sequqnce of all elements|   O(n log n)      |
=             | compares with other sequence    |   O(n log n)      |
toString      | returns string representation   |   O(n log n)      |


### Examples

We can use min-heap or max-heap to create a heap:
```clojure
=> (min-heap 9 7 5 3)  
#<LeftistHeap 3 5 7 9>
=> (max-heap 9 7 5 3)  
#<LeftistHeap 9 7 5 3>
```

No-args factory functions return empty heap:
```clojure
=> (empty? (max-heap))  
true
```

We can use custom comparator function:
```clojure
=> (heap #(<= (count %1) (count %2)) "hkjlm" "defg" "abc")
#<LeftistHeap abc defg hkjlm>
```

conj adds an element to heap:
```clojure
=> (conj (heap #(<= (count %1) (count %2))) "heaps!!!")
#<LeftistHeap heaps!!!>
```

pop returns heap with the smallest element removed:
```clojure
(pop (heap #(<= (count %1) (count %2)) "hkjlm" "defg" "abc"))
#<LeftistHeap defg hkjlm>
```

Smallest element:
```clojure
=> (peek (max-heap 9 8 7 6))
9
```

seq returns all elements in sorted order:
```clojure
=> (seq (min-heap 9 7 5 3))
(3 5 7 9)
```

Heaps are countable:
```clojure
=> (count (max-heap 9 7 5 3) ) 
4
```

## License

Copyright © 2015 Maris

Distributed under the Eclipse Public License either version 1.0 

