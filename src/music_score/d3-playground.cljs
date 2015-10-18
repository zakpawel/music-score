(ns music-score.d3-playground
  (:require [cljs.core.async :as async]
            [clojure.data :as data]
            [cljs.pprint :as pprint :refer [pprint]]
            [rum]
            [datascript.core :as d]
            [cljsjs.d3]
            [music-score.ui :as ui]
            [music-score.abc :as abc]
            [music-score.vex :as vex]
            [jamesmacaulay.zelkova.signal :as z])
  (:require-macros [cljs.core.async.macros :as async]))

(enable-console-print!)

(println "Edits to this text should show up in your developer console.")

;; play with d3

(def id (let [huhu (atom 0)]
          (fn [] (swap! huhu inc)
            @huhu)))

(defn generate-circle [width height]
  #js {:id   (id)
       :r    (+ 10 (rand-int 20))
       :x    (rand-int width)
       :y    (rand-int height)
       :fill (str "rgb(" (rand-int 256) "," (rand-int 256) "," (rand-int 256) ")")})

(declare render-d3)

(let [w 300
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
