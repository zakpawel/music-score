(ns music-score.ui
    (:require [cljs.core.async :as async]
              [clojure.data :as data]
              [music-score.abc :as abc]
              [music-score.vex :as vex]
              [music-score.d3playground :as d3]
              [music-score.keyboard :as keyboard]
              [music-score.start-app :as start]
              [rum.core :as rum :refer-macros [defc defcs]]
              [jamesmacaulay.zelkova.signal :as z])
    (:require-macros
      [music-score.utils :as utils :refer [print debug]]
      [cljs.core.async.macros :as async]))

(enable-console-print!)

(defonce drag-channel (z/write-port [::nothing [0 0]]))

(extend-type js/HTMLCollection
  ISeqable
  (-seq [array] (array-seq array 0)))

(defn get-evt-coord [e]
  [(.. e -clientX) (.. e -clientY)])


(defn vertical-update [{:keys [start-value start-x start-y] :as state} x y]
  (println "vertical-update still alive")
  (-> state
      (update :value #(- start-value (Math/floor (* 0.45 (- y start-y)))))))

(defn horizontal-update [{:keys [start-value start-x start-y] :as state} x y]
  (println "horizontal-update still alive")
  (-> state
      (update :value #(+ start-value (Math/floor (* 0.45 (- x start-x)))))))

(defn mk-update-state
  ([key]
    (mk-update-state key horizontal-update))
  ([key update-fn]
    (fn update-state [state [event [x y] mkey]]
      (let [ret (condp = event
                  :drag-start (if (= key mkey) (do
                                                 #_(print "mousedown received")
                                                 (-> state
                                                     (assoc :dragging true)
                                                     (assoc :start-x x)
                                                     (assoc :start-y y)
                                                     (assoc :start-value (state :value))))
                                               state)
                  :mouse-move (if (state :dragging)
                                (update-fn state x y)
                                state)
                  :drag-end (do
                              #_(print "mouseup received")
                              (assoc state :dragging false)))]
        (->> ret
             (clj->js)
             #_(.log js/console "update-state" (str event)))
        ret))))

(defn foldp [f init ch]
  (let [out (async/chan)]
    (async/go-loop [st init]
      (let [x (<! ch)
            new-st (f st x)]
        (>! out new-st)
        (recur new-st)))
    out))


(defcs number-field <
       {:did-mount
        (fn [state]
          (println "number-field did-mount")
          (let [{:keys [value
                        key
                        on-drag-change
                        on-value-change
                        min
                        max]} (-> state (:rum/args) (first))
                initial-state {:dragging false :value value}
                out-ch (foldp (mk-update-state key) initial-state (z/to-chan drag-channel))]
            (async/go-loop [old-state initial-state]
                           (let [new-state (<! out-ch)
                                 old-value (:value old-state)
                                 new-value (:value new-state)
                                 old-drag (:dragging old-state)
                                 new-drag (:dragging new-state)]
                             (when (and (not= old-value new-value)
                                        (or (nil? min) (> new-value min))
                                        (or (nil? max) (< new-value max)))
                               (on-value-change new-value))
                             (when (not= old-drag new-drag)
                               (on-drag-change new-drag))
                             (recur new-state)))
            state))
        :transfer-state
        (fn [old-state state]
          (merge state (select-keys old-state [::drag-chan])))}
 [state {:keys [on-blur value key]} channel]
 [:div.number-field
  {:on-mouse-down (fn [e]
                    (print "mousedown" value)
                    (async/put! drag-channel [:drag-start (get-evt-coord e) key]) nil)}
  (->> value
       (vex/midi-to-my-note)
       ((fn [{:keys [pitch acc octave]}] (str (name pitch) (if acc (name acc) "") octave))))])

(defn render-key [n oct-num type channel]
  (let [value (+ (+ n 12) (* oct-num 12))]
    (if (= type :white)
    	[:div {:class "key white"
         		:on-click #(async/put! channel [:key-press value])}]
        [:div {:class "key black"
         	   :style {:left (str (* n 12) "px")}
         	   :on-click #(async/put! channel [:key-press value])}])))


(defn parse-pitch [evt]
  (let [x (as-> (.. evt -target -value) _
                (re-seq #"(\d+)" _)
                (-> _
                  (first)
                  (rest))
                (map #(js/parseInt %) _))]
        (println "parse-pitch" x)
        (first x)
    ))

(declare render-notes)
(declare query-dom-notes)

(defc render-range-notes <
      {:did-update
       (fn [state]
         (println "render-range-notes did-udpate")
         (let [node (->> state
                         (:rum/react-component)
                         (.getDOMNode))
               notes (query-dom-notes node)
               low (first notes)
               high (second notes)
               add-handler (fn [el key]
                             (when el
                               (.addEventListener
                                 (.-nextSibling el)
                                 "mousedown"
                                 (fn [e]
                                   (async/put! drag-channel [:drag-start (get-evt-coord e) key]) nil))))]
           (add-handler low :low)
           (add-handler high :high)
           state
           ))}
  [low-note high-note clef]
  (render-notes [[low-note nil] [high-note nil]] clef "config" 300))

(defn render-range-config [low-note high-note clef channel]
  [:div.range-config
   (number-field {:min 20 :max 108
                  :value low-note
                  :on-blur #(async/put! channel [:core.action/config :range 0 (parse-pitch %)])
                  :key :low
                  :on-value-change #(async/put! channel [:core.action/config :range 0 %])
                  :on-drag-change #(async/put! channel [:core.action/dragging %])})
   (number-field {:min 20 :max 108
                  :value high-note
                  :key :high
                  :on-blur #(async/put! channel [:core.action/config :range 1 (parse-pitch %)])
                  :on-value-change #(async/put! channel [:core.action/config :range 1 %])
                  :on-drag-change #(async/put! channel [:core.action/dragging %])})

   #_(render-range-notes low-note high-note clef)]
  )

(defn render-configuration [model channel]
  (let [clef (get-in model [:config :key])
        switch (fn [clef]
                 (condp = clef
                   :treble :bass
                   :bass :treble
                   ))
        low-note (-> model (get-in [:config :range 0]))
        high-note (-> model (get-in [:config :range 1]))]
        [:div.config
         [:div.clefs.cell
          {:on-click #(async/put! channel [:core.action/config :key (switch clef)])} (name clef)]
          (render-range-config low-note high-note clef channel)]
    ))

(defc render-exercise <
  rum/static
  [guesses clef]
      [:div
       (render-notes guesses clef "exercise")])

(defn printr [e]
  (print e)
  e)

(defn render-abc [state lifecycle]
  (let [[midi clef] (-> state (:rum/args))
        node (-> state
                 (:rum/react-component)
                 (.getDOMNode))
        to-abc (fn to-abc [midi] (->> midi
                                      (filter #(not (nil? %)))
                                   (map (fn [e]
                                          (if (vector? e)
                                            (let [c (count e)]
                                              (if (= 1 c)
                                                (abc/midi-to-abc (first c))
                                                (let [[q a] e]
                                                  (if (= q a)
                                                    (abc/midi-to-abc q)
                                                    (str "[" (to-abc e) "]")))))
                                            (abc/midi-to-abc e))))
                                   (apply str)))
        abc (->> (to-abc midi) (abc/add-clef clef))]
    (print "render-abc" #_abc lifecycle (-> state (:rum/args)))
    (abc/render-abc abc node)
    state))


(defn render-vex [state lifecycle]
  (let [[midi clef id width] (-> state (:rum/args))
        node (-> state
                 (:rum/react-component)
                 (.getDOMNode))]
    (if width
      (vex/render-vex node midi clef width)
      (vex/render-vex node midi clef))
    )
  )

(defc render-notes <
      {:should-update
       (fn [old-state new-state]
         (let [neq (not= (:rum/args old-state) (:rum/args new-state))]
           (print "render-notes should-update" neq (-> old-state (:rum/args)) (-> new-state (:rum/args)))
           neq))
       :did-update
       (fn [state]
         (render-vex state "did-update")
         state)
       :did-mount
       (fn [state]
         (render-vex state "did-mount")
         state)}
  [midi clef id width]
  [:div {:id id}])


(rum/defc root-component <
          {:did-update
           (fn [state]
             (print "root-component did-update")
             state)}
  [input-signal state]
  (let [{:keys [question
                answer
                guesses
                dragging?
                keyboard
                config
                configuring?
                chart-data]} state
        

        clef (get-in state [:config :key])
        cls (if dragging?
              "dragging" "")]
    (print "root-component render")
    [:div {:id             "container"
           :class          cls
           :on-mouse-move  #(do (async/put! drag-channel [:mouse-move (get-evt-coord %)]) nil)
           :on-mouse-up    #(do (print "onmouseup") (async/put! drag-channel [:drag-end (get-evt-coord %)]) nil)
           :on-mouse-leave #(do (print "onmouseleave") (async/put! drag-channel [:drag-end (get-evt-coord %)]) nil)}
     [:div {:id "sidebar"}
      (render-configuration state input-signal)]
     (render-exercise guesses clef)
     (keyboard/render-keyboard (start/proxy #(start/make-action :core.action/keyboard %) input-signal)
                               #_(start/map-signal #(start/make-action :core.action/keyboard %) input-signal)
                               keyboard
                               config
                               configuring?
                               chart-data)
     #_(render-keyboard state input-signal)
     [:div#vex]]))