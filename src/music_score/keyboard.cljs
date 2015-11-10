(ns music-score.keyboard
  (:require [cljs.core.async :as async]
            [clojure.data :as data]
            [clojure.string :as s]
            [cljs.pprint :as pprint :refer [pprint]]
            [rum]
            [datascript.core :as d]
            [cljsjs.d3]
    #_[music-score.ui :as ui]                               ;; causes cyclic deps, figwheel?
            [music-score.abc :as abc]
            [music-score.vex :as vex]
            [jamesmacaulay.zelkova.signal :as z]
            [music-score.start-app :as start])
  (:require-macros [cljs.core.async.macros :as async]
                   [rum :refer [defc defcs]]
                   [music-score.utils :refer [debug]]))



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
                                    #js {:pitch p :offset offset :kind color})) d))
               ]
           (fn [node channel model {[l h] :range}]
             (println "draw-keyboard!")
             (let [svg (-> js/d3
                           (.select node))
                   selection
                   (if (model :dragging)
                     (model :selection)
                     (into #{} (range l (inc h))))

                   {:keys [black white]}
                   (as-> data2 data2
                         (map (fn [d]
                                (aset d "inrange" (selection (.-pitch d)))
                                d) data2)
                         (group-by #(.-kind %) data2))

                   white (clj->js white)
                   black (clj->js black)

                   attach-events!
                   (fn [d3]
                     (-> d3
                         (.on "click" (fn [a]
                                        (async/put! channel
                                                    (start/make-action
                                                      :keyboard.action/key-press (.-pitch a)))))
                         (.on "mousedown" (fn [a]
                                            (async/put! channel
                                                        (start/make-action
                                                          :keyboard.action/key-down (.-pitch a)))))
                         (.on "mouseup" (fn [a]
                                          (async/put! channel
                                                      (start/make-action
                                                        :keyboard.action/key-up (.-pitch a)))))
                         (.on "mouseover" (fn [a]
                                            (async/put! channel
                                                        (start/make-action
                                                          :keyboard.action/key-over (.-pitch a)))))
                         (.on "mouseout" (fn [a]
                                           (async/put! channel
                                                       (start/make-action
                                                         :keyboard.action/key-out (.-pitch a)))))))
                   select! (fn [selector data]
                             (-> svg
                                 (.selectAll selector)
                                 (.data (clj->js data) (fn [d] (.-pitch d)))))

                   draw-key! (fn [selector class data stroke w h rx ry]
                               (let [selection (select! selector data)]
                                 (-> selection
                                     (.enter)
                                     (.append "rect")
                                     (.attr "x" (fn [d i] (.-offset d)))
                                     (.attr "rx" rx)
                                     (.attr "ry" ry)
                                     (.attr "stroke" stroke)
                                     (attach-events!)
                                     (.attr "width" w)
                                     (.attr "height" h))
                                 (-> selection
                                     (.attr "class" class))))

                   draw-white! #(draw-key! %1 %2 %3 "#333333"
                                           20 100 4 4)

                   draw-black! #(draw-key! %1 %2 %3 "black"
                                           10 60 1 1)

                   compute-class (fn [base-class off-class]
                                   (fn [d]
                                     (if (.-inrange d)
                                       (str base-class " " off-class)
                                       base-class)))

                   ]
               (println "draw-keyboard" selection)

               (draw-white! "rect.key"
                            "white key"
                            white)
               (draw-white! "rect.mask"
                            (compute-class "white key mask" "off")
                            white)

               (draw-black! "rect.key"
                            "black key"
                            black)
               (draw-black! "rect.mask"
                            (compute-class "black key mask" "off")
                            black))
             )))

(defn render-keyboard-hook [state]
  (let [[channel model config & r] (->> state
                                 (:rum/args))]
    (->> state (:rum/args) (second) (debug))
    (as-> state x
          (:rum/react-component x)
          (.getDOMNode x)
          (.-firstChild x)
          (draw-keyboard! x channel model config)))
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
      [signal data config]
      (do
        (println "render-keyboard render")
        [:div#keybd
         [:svg {:width 1400 :height 400}]]))

(defonce initial-model
         {:range {:start 21
                  :end   108}
          :dragging false
          :selection-start nil
          :selection #{}})

(defonce signal (z/write-port ::nothing))

(defn add-to-selection [model pitch]
  (if (model :dragging)
    (let [[l h] (sort (vector (model :selection-start)
                              pitch))]
      (-> model
          (assoc-in [:selection] (into #{} (range l (inc h))))))
    model))

(defn update-model [action model]
  (println "keyboard update-model" action model)
  (let [[action-name pitch & r] action

        new-model
        (condp = action-name
          :keyboard.action/key-over
          (add-to-selection model pitch)

          :keyboard.action/key-out
          (-> model
              (update-in [:selection] disj pitch))

          :keyboard.action/key-down
          (-> model
              (assoc-in [:selection-start] pitch)
              (assoc-in [:dragging] true)
              (add-to-selection pitch))

          :keyboard.action/key-up
          (-> model
              (add-to-selection pitch)
              ((fn [model] (async/put! signal
                                       (start/make-action
                                         :keyboard.action/selection
                                         (:selection model)))
                 model))
              (assoc-in [:dragging] false)
              (assoc-in [:selection] #{}))
          model)]
    (println "keyboard new-model" new-model)
    new-model))