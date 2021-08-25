(ns user
  (:require
    [clojure.spec.alpha :as s]
    [com.rpl.specter :refer [ALL collect-one compact defprotocolpath
                             END extend-protocolpath filterer if-path
                             LAST NONE recursive-path select selected? setval subselect
                             srange STAY transform MAP-VALS pred>= putval walker view]]
    ))

(transform [MAP-VALS MAP-VALS]
           inc
           {:a {:aa 1} :b {:ba -1 :bb 2}})

(transform [ALL :a even?]
           inc
           [{:a 1} {:a 2} {:a 4} {:a 3}])

(select [ALL ALL #(= 0 (mod % 3))]
        [[1 2 3 4] [] [5 3 2 18] [2 4 6] [12]])

(transform [(filterer odd?) LAST]
           inc
           [2 1 3 6 9 4 8])

(setval [:a ALL nil?] NONE {:a [1 2 nil 3 nil]})

(setval [:a :b :c] NONE {:a {:b {:c 1}}})

(setval [:a (compact :b :c)] NONE {:a {:b {:c 1}}})

(transform [(srange 1 4) ALL odd?] inc [0 1 2 3 4 5 6 7])

(setval (srange 2 4) [:a :b :c :d :e] [0 1 2 3 4 5 6 7 8 9])

(setval [ALL END] [:a :b] [[1] '(1 2) [:c]])

(select (walker keyword?)
        {3 [1 2 [6 7]] :a 4 :c {:a 5 :d [9 nil]}})

(transform [(srange 4 11) (filterer even?)]
           reverse
           [0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15])

(setval [ALL
         (selected? (filterer even?) (view count) (pred>= 2))
         END]
        [:c :d]
        [[1 2 3 4 5 6] [7 0 -1] [8 8] []])

(comment "When doing more involved transformations,
you often find you lose context when navigating deep
 within a data structure and need information \"up\"
 the data structure to perform the transformation.
 Specter solves this problem by allowing you to
 collect values during navigation to use in the transform function.
  Here's an example which transforms a sequence of maps by adding the
   value of the :b key to the value of the :a key,
    but only if the :a key is even:\n\n")

(transform [ALL (collect-one :b) :a even?]
           +
           [{:a 1 :b 3} {:a 2 :b -10} {:a 4 :b 10} {:a 3}])

(comment
  The four built-in ways for collecting values are VAL,
  collect, collect-one, and putval. VAL just adds whatever
  element it's currently on to the value list, while collect
  and collect-one take in a selector to navigate to the desired
  value. collect works just like select by finding a sequence of values,
  while collect-one expects to only navigate to a single value. Finally,
  putval adds an external value into the collected values list.)

(transform [:a (putval 10)]
           +
           {:a 1 :b 3})

(transform [ALL (if-path [:a even?] [:c ALL] :d)]
           inc
           [{:a 2 :c [1 2] :d 4} {:a 4 :c [0 10 -1]} {:a -1 :c [1 1 1] :d 1}])

(comment
  The next examples demonstrate recursive navigation.
  Here's one way to double all the even numbers in a tree.
  )

(defprotocolpath TreeWalker [])

(extend-protocolpath TreeWalker
                     Object nil
                     clojure.lang.PersistentVector [ALL TreeWalker])

(transform [TreeWalker number? even?] #(* 2 %) [:a 1 [2 [[[3]]] :e] [4 5 [6 7]]])

(def TreeValues
  (recursive-path [] p
                  (if-path vector?
                           [ALL p]
                           STAY
                           )))

(comment
  Here's how to reverse the positions of all even numbers in a tree (with order based on a depth first search).
  This example uses conditional navigation instead of protocol paths to do the walk)

(transform (subselect TreeValues even?)
           reverse
           [1 2 [3 [[4]] 5] [6 [7 8] 9 [[10]]]]
           )
