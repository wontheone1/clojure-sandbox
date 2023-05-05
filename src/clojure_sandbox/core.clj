(ns clojure-sandbox.core
  (:require
   [dewise.planner.lib.pogo.api :as pogo]
   [dewise.planner.lib.util :as util]
   [loco.core :as loco]))

#_ (ns dewise.planner.lib.atedu.rostering.loco
  (:require
   [dewise.planner.lib.pogo.api :as pogo]
   [dewise.planner.lib.util :as util]
   [loco.core :as loco]))

(defn format-solution
  [vars]
  (->> vars
       (remove (comp zero? val))
       keys
       (map (fn [[_ class-id teacher-id]] {:class class-id :teacher teacher-id}))))

(defn formulate
  [{:keys [classes teachers] :as problem}]
  (let [vars (for [{class-id :id subject :subject} classes ; Create binary assignment vars from each class to the teachers that have the required skill.
                   {teacher-id :id subjects :subjects} teachers
                   :when (subjects subject)]
               [:x class-id teacher-id])
        get-class-id second
        get-teacher-id util/third
        classes-by-id (util/index-by :id classes)
        teachers-by-id (util/index-by :id teachers)
        vars-by-class-id (group-by get-class-id vars)
        vars-by-teacher-id (group-by get-teacher-id vars)
        vars-by-teacher&block (->> vars (group-by (juxt get-teacher-id (comp :block classes-by-id get-class-id))))
        ;; relations:
        vars-are-binaryo (->> vars (map (fn [var] [:$in var 0 1])))
        classes-are-assignedo (->> vars-by-class-id
                                   (map (fn [[_ vars]]
                                          [:$= 1 (into [:$+] vars)])))
        teachers-are-not-overloadedo (->> vars-by-teacher-id
                                          (map (fn [[teacher vars]]
                                                 (let [capacity-of-teacher (:capacity (teachers-by-id teacher))
                                                       total-load (->> vars
                                                                       (map (fn [[_ class-id _ :as var]]
                                                                              [:$* var (:load (classes-by-id class-id))]))
                                                                       (into [:$+]))]
                                                   [:$<= total-load capacity-of-teacher]))))
        block-constrainto (->> vars-by-teacher&block
                               (keep (fn [[[teacher block] vars]]
                                       (when (and block (second vars))
                                         [:$>= 1 (into [:$+] vars)]))))]
    (vec (concat vars-are-binaryo
                 classes-are-assignedo
                 teachers-are-not-overloadedo
                 block-constrainto))))

(defn solve
  [problem]
  (-> (formulate problem)
      (pogo/->loco)
      (loco/solution)
      (format-solution)))

(comment

 (formulate {:classes [{:id 0 :subject "da" :load 42 :block 1}
                       {:id 1 :subject "da" :load 42 :block 1}
                       {:id 2 :subject "ma" :load 42}
                       {:id 3 :subject "en" :load 30}]
             :teachers [{:id 0 :subjects #{"da" "en"} :capacity 50}
                        {:id 1 :subjects #{"da"} :capacity 50}
                        {:id 2 :subjects #{"da"} :capacity 50}
                        {:id 3 :subjects #{"ma"} :capacity 50}]})
 =>
 [[:$in [:x 0 0] 0 1]
  [:$in [:x 0 1] 0 1]
  [:$in [:x 0 2] 0 1]
  [:$in [:x 1 0] 0 1]
  [:$in [:x 1 1] 0 1]
  [:$in [:x 1 2] 0 1]
  [:$in [:x 2 3] 0 1]
  [:$in [:x 3 0] 0 1]
  [:$= 1 [:$+ [:x 0 0] [:x 0 1] [:x 0 2]]]
  [:$= 1 [:$+ [:x 1 0] [:x 1 1] [:x 1 2]]]
  [:$= 1 [:$+ [:x 2 3]]]
  [:$= 1 [:$+ [:x 3 0]]]
  [:$<= [:$+ [:$* [:x 0 0] 42] [:$* [:x 1 0] 42] [:$* [:x 3 0] 30]] 50]
  [:$<= [:$+ [:$* [:x 0 1] 42] [:$* [:x 1 1] 42]] 50]
  [:$<= [:$+ [:$* [:x 0 2] 42] [:$* [:x 1 2] 42]] 50]
  [:$<= [:$+ [:$* [:x 2 3] 42]] 50]
  [:$>= 1 [:$+ [:x 0 0] [:x 1 0]]]
  [:$>= 1 [:$+ [:x 0 1] [:x 1 1]]]
  [:$>= 1 [:$+ [:x 0 2] [:x 1 2]]]]

 (solve {:classes [{:id 0 :subject "da" :load 42 :block 1}
                   {:id 1 :subject "da" :load 42 :block 1}
                   {:id 2 :subject "ma" :load 42}
                   {:id 3 :subject "en" :load 30}]
         :teachers [{:id 0 :subjects #{"da" "en"} :capacity 50}
                    {:id 1 :subjects #{"da"} :capacity 50}
                    {:id 2 :subjects #{"da"} :capacity 50}
                    {:id 3 :subjects #{"ma"} :capacity 50}]})
 => ({:class 0, :teacher 2} {:class 1, :teacher 1} {:class 2, :teacher 3} {:class 3, :teacher 0})
 )

; just give one solution honoring :load and :capacity, assign teachers to classes

(comment

 (defn at-mosto [max-sum vars])
 (< sum max)
 (defn sumo [arguments res])

 )
