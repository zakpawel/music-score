(ns music-score.d3playground
  (:require [cljs.core.async :as async]
            [clojure.data :as data]
            [cljs.pprint :as pprint :refer [pprint]]
            [rum]
            [datascript.core :as d]
            [cljsjs.d3]
    #_[music-score.ui :as ui]                               ;; causes cyclic deps, figwheel?
            [music-score.abc :as abc]
            [music-score.vex :as vex]
            [jamesmacaulay.zelkova.signal :as z])
  (:require-macros [cljs.core.async.macros :as async]
                   [rum :refer [defc defcs]]
                   [music-score.utils :refer [debug]]))

(enable-console-print!)

(println "Edits to this text should show up in your developer console.")





;; play with d3

(defonce id (let [huhu (atom 0)]
              (fn [] (swap! huhu inc)
                @huhu)))

(defn generate-circle [width height]
  #js {:id   (id)
       :r    (+ 10 (rand-int 20))
       :x    (rand-int width)
       :y    (rand-int height)
       :fill (str "rgb(" (rand-int 256) "," (rand-int 256) "," (rand-int 256) ")")})

(declare render-d3)

#_(let [w 300
        h 150
        svg (-> js/d3
                (.select "body")
                (.append "svg")
                (.attr "id" "dots"))]
    (async/go-loop [data #js []]
                   (.push data (generate-circle w h))
                   (when (> (.. data -length) 3)
                     (.shift data)
                     )
                   (async/<! (async/timeout 1000))
                   (render-d3 svg data)
                   (recur data)))


(defn render-d3 [svg data]
  (let [c (-> svg
              (.selectAll "circle")
              (.data data (fn [d] (.-id d))))]
    (-> c
        (.enter)
        (.append "circle")
        (.attr "opacity" 0)
        (.attr "r" 0)
        (.attr "cx" (fn [d] (.-x d)))
        (.attr "cy" (fn [d] (.-y d)))
        (.attr "fill" (fn [d] (.-fill d)))
        (.transition)
        (.attr "r" (fn [d] (.-r d)))
        (.attr "opacity" 1))

    (-> c
        (.exit)
        (.transition)
        (.attr "opacity" 0)
        (.attr "r" 0))))

(defn bar [c]
  (let [g (-> c
              (.enter)
              (.append "svg:g"))]

    (-> g
        (.attr "height" (fn [d] (print "draw-chart" d) (->> (.-count d)
                                                            (* 10))))
        (.attr "width" 20)
        (.attr "x" (fn [d i] (* i 20)))
        (.attr "fill" "dodgerblue"))))


(defonce draw-chart!
         (let [svg (atom nil)]
           (fn [data]
             (when (not @svg)
               (reset! svg (-> js/d3
                               (.select "div#container")
                               (.append "svg")
                               (.attr "width" 800)
                               (.attr "height" 200))))
             (println "draw-chart" data)

             (let [g (-> @svg
                         (.selectAll "g")
                         (.data data (fn [d] (.-note d)))
                         (.enter)
                         (.append "g"))
                   x (-> g
                         (.append "rect")
                         (.attr "class" "bar-count"))
                   x (-> g
                         (.append "rect")
                         (.attr "class" "bar-correct"))
                   x (-> g (.append "text"))
                   t (-> @svg
                         (.selectAll "text")
                         (.data data (fn [d] (.-note d)))
                         (.attr "x" (fn [d i] (* i 20)))
                         (.attr "dy" 10)
                         (.attr "class", "bar-label")
                         (.text (fn [d] (str (.-note d)))))

                   b (-> @svg
                         (.selectAll "rect.bar-correct")
                         (.data data (fn [d] (.-note d)))

                         (.attr "height" (fn [d] (->> (.-correct d)
                                                      (* 10))))
                         (.attr "width" 20)
                         (.attr "y" 12)
                         (.attr "x" (fn [d i] (* i 20)))
                         (.attr "fill" "dodgerblue"))

                   b (-> @svg
                         (.selectAll "rect.bar-count")
                         (.data data (fn [d] (.-note d)))

                         (.attr "height" (fn [d] (->> (.-count d)
                                                      (* 10))))
                         (.attr "width" 20)
                         (.attr "y" 12)
                         (.attr "x" (fn [d i] (* i 20)))
                         (.attr "fill" "steelblue"))]))))

