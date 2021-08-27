(ns user
  (:require
    [clojure.spec.alpha :as s]
    [com.rpl.specter :refer [ALL collect-one compact defprotocolpath
                             END extend-protocolpath filterer if-path
                             LAST NONE recursive-path select selected? setval subselect
                             srange STAY transform MAP-VALS pred>= putval walker view]]
    ))

[:heart 1]
[:diamond 13]

(def poker-deck
  (for [suit [:heart :diamond :spade :clober]
        rank (range 1 14)
        ]
    [suit rank]))

(defn get-hand [poker-deck]
  (->> poker-deck
       (shuffle)
       (take 5)))

(defn have-expected-number-of-rank-pair? [hand expected-num rank]
  (let [pairnum->pairs (->> (get-hand hand)
                            (group-by (fn [[suit rank]] rank))
                            (map (fn [[number pairs]] {:num-pairs (count pairs)
                                                       :number number}))
                            (group-by (fn [{:keys [num-pairs number]}] num-pairs))

                            )]
    (println :hand hand)
    (println pairnum->pairs)
    (boolean (= expected-num (count (pairnum->pairs rank))))))

(defn have-pair? [hand]
  (have-expected-number-of-rank-pair? hand 1 2))

(defn have-two-pairs? [hand]
  (have-expected-number-of-rank-pair? hand 2 2))

(have-two-pairs? '([:spade 12]  [:diamond 12] [:heart 3] [:clober 3] [:diamond 7]))
(have-two-pairs? '([:spade 12]  [:diamond 12] [:heart 3] [:clober 4] [:diamond 7]))

(defn have-triple? [hand]
  (have-expected-number-of-rank-pair? hand 1 3))

(defn have-four-card? [hand]
  (have-expected-number-of-rank-pair? hand 1 4))

(have-pair? '([:spade 12]  [:diamond 12] [:heart 3] [:clober 4] [:diamond 7]))
(have-pair? '([:spade 12]  [:diamond 12] [:heart 3] [:clober 3] [:diamond 7]))
(have-triple? '([:spade 12]  [:diamond 12] [:heart 12] [:clober 3] [:diamond 7]))
(have-triple? '([:spade 12]  [:diamond 12] [:heart 3] [:clober 3] [:diamond 7]))

(defn have-full-house? [hand]
  (and (have-triple? hand) (have-pair? hand)))

(have-full-house? '([:spade 12]  [:diamond 12] [:heart 12] [:clober 3] [:diamond 3]))
(have-full-house? '([:spade 12]  [:diamond 13] [:heart 12] [:clober 3] [:diamond 3]))

(defn have-flush? [hand]
  (->> hand
       (group-by (fn [[suit rank]] suit))
       (count)
       (= 1)))

(defn have-straight? [hand]
  (let [sorted-ranks (sort (map (fn [[suit rank]] rank) hand))
        minimum-rank (first sorted-ranks)
        straignt-range (range minimum-rank (+ minimum-rank 5))]
    (= straignt-range sorted-ranks)))

(have-straight? '([:heart 12]  [:heart 10] [:heart 12] [:heart 3] [:heart 3]))
(have-straight? '([:heart 13]  [:heart 1] [:heart 2] [:heart 3] [:heart 4]))

(have-flush? '([:heart 12]  [:heart 10] [:heart 12] [:heart 3] [:heart 3]))
(have-flush? '([:heart 12]  [:diamond 10] [:heart 12] [:heart 3] [:heart 3]))

(defn evaluate-hand [hand]
  (cond
    (have-straight? hand)
    :straight

    (have-four-card? hand)
    :four-card

    (have-full-house? hand)
    :full-house

    (have-flush? hand)
    :flush

    (have-triple? hand)
    :triple

    (have-two-pairs? hand)
    :two-pairs

    (have-pair? hand)
    :pair

    :else
    :high-card
    ))

#_(defn have-pair? [hand]
  (let [pairnum->pairs (->> (get-hand hand)
                            (group-by (fn [[suit rank]] rank))
                            (map (fn [[number pairs]] {:num-pairs (count pairs)
                                                       :number number}))
                            (group-by (fn [{:keys [num-pairs number]}] num-pairs))

                            )]
    (println :hand hand)
    (println pairnum->pairs)
    (boolean (= 1 (count (pairnum->pairs 2))))))



'([:spade 12]  [:diamond 12] [:heart 3] [:clober 4] [:diamond 7])
'([:spade 12]  [:diamond 12] [:heart 3] [:clober 3] [:diamond 7])
