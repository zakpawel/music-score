(ns music-score.ui
    (:require [cljs.core.async :as async]
              [clojure.data :as data]
              [music-score.abc :as abc]
              [music-score.vex :as vex]
              [music-score.d3playground :as d3]
              [rum]
              [jamesmacaulay.zelkova.signal :as z])
    (:require-macros
      [music-score.utils :as utils :refer [debug]]
      [cljs.core.async.macros :as async]
      [rum :refer [defc defcs]]))

(enable-console-print!)

(defonce drag-channel (z/write-port [:nothing [0 0]]))

(def octave-keys
  [:white
    :black
   :white
    :black
   :white
   :white
    :black
   :white
    :black
   :white
    :black
   :white])

#_(debug (+ 2 2))

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

(defn render-octave [channel oct-num]
  (as-> octave-keys _
    (map-indexed (fn [n k]
                  (render-key n oct-num k channel)) _)
    [:div {:className "octave"} _]))


(defc render-keyboard [data channel]
	[:div {:id "keyboard"}
		(map #(render-octave channel %) (range 1 8))])

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
                  :on-blur #(async/put! channel [:config :range 0 (parse-pitch %)])
                  :key :low
                  :on-value-change #(async/put! channel [:config :range 0 %])
                  :on-drag-change #(async/put! channel [:dragging %])})
   (number-field {:min 20 :max 108
                  :value high-note
                  :key :high
                  :on-blur #(async/put! channel [:config :range 0 (parse-pitch %)])
                  :on-value-change #(async/put! channel [:config :range 1 %])
                  :on-drag-change #(async/put! channel [:dragging %])})

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
          {:on-click #(async/put! channel [:config :key (switch clef)])} (name clef)]
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
             (println "root-component did-update")
             state)}
  [state input-signal]
  (let [clef (get-in state [:config :key])
        question (state :question)
        answer (state :answer)
        guesses (state :guesses)
        dragging (state :dragging)
        cls (if dragging
              "dragging" "")]
    (println "render root-component")
    [:div {:id             "container"
           :class          cls
           :on-mouse-move  #(do (async/put! drag-channel [:mouse-move (get-evt-coord %)]) nil)
           :on-mouse-up    #(do (print "onmouseup") (async/put! drag-channel [:drag-end (get-evt-coord %)]) nil)
           :on-mouse-leave #(do (print "onmouseleave") (async/put! drag-channel [:drag-end (get-evt-coord %)]) nil)}
     [:div {:id "sidebar"}
      (render-configuration state input-signal)]
     (render-exercise guesses clef)
     (d3/render-keyboard input-signal)
     #_(render-keyboard state input-signal)
     [:div#vex]]))