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

(declare draw-keyboard!)

(defn render-keyboard-hook [state]
  (let [channel (->> state
                     (:rum/args)
                     (first))]
    (as-> state x
          (:rum/react-component x)
          (.getDOMNode x)
          (debug x)
          (.-firstChild x)
          (debug x)
          (draw-keyboard! x channel)))
  state)

(defc render-keyboard <
      {:did-mount
       (fn [state]
         (println "render-keyboard did-mount")
         (render-keyboard-hook state))
       :did-update
       (fn [state]
         (println "render-keyboard did-update")
         (render-keyboard-hook state))}
      [data]
      (do
        (println "render-keyboard render")
        [:div#keybd
         [:svg {:width 1400 :height 400}]]))



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


(defn div-with-rem
  ([n d]
   (div-with-rem n d 12))
  ([n d padding]
   (as-> n n
         (- n padding)
         (vector (Math/floor (/ n d)) (rem n d)))))

(defn white-count [n]
  (let [m {0  [0 :white]
           1  [1 :black]
           2  [1 :white]
           3  [2 :black]
           4  [2 :white]
           5  [3 :white]
           6  [4 :black]
           7  [4 :white]
           8  [5 :black]
           9  [5 :white]
           10 [6 :black]
           11 [6 :white]}
        [o n] (div-with-rem n 12)
        [n color] (m n)]
    (vector (+ (* o 7) n) color))
  )

(defn key-offset [p key-width]
  (let [[wc color] (white-count p)
        padding (if (= color :black) (- 0 (/ 20 4)) 0)
        offset (+ (* wc key-width) padding)]
    [offset color]))

(defonce draw-keyboard!
   (let [start 21
         width 20
         [start-offset _] (key-offset start width)
         domain (range start 109)
         data2 (as-> domain d
                     (map (fn [p]
                            (let [[offset color] (key-offset p 20)
                                  offset (- offset start-offset)]
                              #js {:pitch p :offset offset :color color})) d))]
     (fn [node channel]
       #_(println "draw-keyboard!")
       (let [svg (-> js/d3
                     (.select node))
             {:keys [black white]} (group-by #(.-color %) data2)]
         (-> svg
             (.selectAll "rect")
             (.data (clj->js white) (fn [d] (.-pitch d)))
             (.enter)
             (.append "rect")
             (.attr "class" "white")
             (.attr "x" (fn [d i] (.-offset d)))
             (.attr "rx" 4)
             (.attr "ry" 4)
             (.attr "stroke" "black")
             (.on "click" (fn [a]
                            (async/put! channel [:key-press (.-pitch a)])))
             (.attr "fill" "white")
             (.attr "width" 20)
             (.attr "height" 100))

         (-> svg
             (.selectAll "rect")
             (.data (clj->js black) (fn [d] (.-pitch d)))
             (.enter)
             (.append "rect")
             (.attr "class" "black")
             (.attr "x" (fn [d i] (.-offset d)))
             (.attr "rx" 1)
             (.attr "ry" 1)
             (.attr "stroke" "black")
             (.on "click" (fn [a]
                            (async/put! channel [:key-press (.-pitch a)])))
             (.attr "fill" "black")
             (.attr "width" 10)
             (.attr "height" 60))
         )
       )))

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

