(ns music-score.ui
    (:require [cljs.core.async :as async]
              [clojure.data :as data]
              [music-score.abc :as abc]
              [rum]
              [jamesmacaulay.zelkova.signal :as z])
    (:require-macros
      [music-score.utils :refer [debug]]
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


(defn mk-update-state [key]
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
                              (-> state
                                  (update :value #(- (state :start-value) (Math/floor (* 0.45 (- y (state :start-y)))))))
                              state)
                :drag-end (do
                            #_(print "mouseup received")
                            (assoc state :dragging false)))]
      (->> ret
           (clj->js)
           #_(.log js/console "update-state" (str event)))
      ret)))

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
          (let [args (-> state (:rum/args) (first))
                value (:value args)
                key (:key args)
                on-drag-change (:on-drag-change args)
                on-value-change (:on-value-change args)
                initial-state {:dragging false :value value}
                out-ch (foldp (mk-update-state key) initial-state (z/to-chan drag-channel))]
            (async/go-loop [old-state initial-state]
                           (let [new-state (<! out-ch)
                                 old-value (:value old-state)
                                 new-value (:value new-state)
                                 old-drag (:dragging old-state)
                                 new-drag (:dragging new-state)]
                             (when (not= old-value new-value)
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
  (abc/midi-to-abc value)])

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
  (render-notes [low-note high-note] clef "config"))

(defn render-range-config [low-note high-note clef channel]
  [:div.range-config.cell
   (number-field {:value low-note :on-blur #(async/put! channel [:config :range 0 (parse-pitch %)])
                  :key :low
                  :on-value-change #(async/put! channel [:config :range 0 %])
                  :on-drag-change #(async/put! channel [:dragging %])})
   (number-field {:value high-note
                  :key :high
                  :on-blur #(async/put! channel [:config :range 1 (parse-pitch %)])
                  :on-value-change #(async/put! channel [:config :range 1 %])
                  :on-drag-change #(async/put! channel [:dragging %])})

   (render-range-notes low-note high-note clef)]
  )

(defn render-configuration [model channel]
  (let [clef (get-in model [:config :key])
        switch {true "selected-clef" false ""}
        low-note (-> model (get-in [:config :range 0]))
        high-note (-> model (get-in [:config :range 1]))]
        [:div.config
         [:div.clefs.cell
          [:div {:class (switch (= :bass clef))   :on-click #(async/put! channel [:config :key :bass])} "K:bass"]
          [:div {:class (switch (= :treble clef)) :on-click #(async/put! channel [:config :key :treble])} "K:treble"]]
          (render-range-config low-note high-note clef channel)]
    ))

(defn check-correctness [question answer]
  (->> (map (fn [q a]
              (if (= q a)
                "correct"
                "error")) question answer)
       (into [])))

(defn query-dom-notes [node]
  (let [dom-notes (. node (getElementsByClassName "note"))]
    dom-notes))

(defn apply-classes! [classes dom-notes]
  (doseq [[clazz note] (map vector classes dom-notes)]
    (let [cls (. note (getAttribute "class"))]
      (. note (setAttribute "class" (str cls " " clazz)))
      )
    )
  )

(defc render-exercise <
  rum/static
      {:did-update
       (fn [state]
         (print "render-exercise did-update")
         (let [[question answer] (-> state (:rum/args))]
           (->> (query-dom-notes (. js/document (getElementById "exercise")))
                (apply-classes! (check-correctness question answer))))
         state)}
  [question answer clef]
      (let [merged (->> (map vector question (concat answer (repeat nil))) (into []))]
        (print "render-exercise render" merged question)
        [:div
         (render-notes merged clef "exercise")
         #_(render-notes answer clef "answer")]))


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

(defc render-notes <
      {:should-update
       (fn [old-state new-state]
         (let [neq (not= (:rum/args old-state) (:rum/args new-state))]
           (print "render-notes should-update" neq (-> old-state (:rum/args)) (-> new-state (:rum/args)))
           neq))
       :did-update
       (fn [state]
         (render-abc state "did-update")
         state)
       :did-mount
       (fn [state]
         (render-abc state "did-mount")
         state)}
  [midi clef id]
  [:div {:id id}])

(rum/defc root-component
  [state input-signal]
    (let [clef (get-in state [:config :key])
          question (state :question)
          answer (state :answer)
          dragging (state :dragging)
          cls (if dragging
                "dragging" "")]
      [:div {:id            "container"
             :class         cls
             :on-mouse-move #(do (async/put! drag-channel [:mouse-move (get-evt-coord %)]) nil)
             :on-mouse-up   #(do (print "onmouseup") (async/put! drag-channel [:drag-end (get-evt-coord %)]) nil)
             :on-mouse-leave #(do (print "onmouseleave") (async/put! drag-channel [:drag-end (get-evt-coord %)]) nil)}
       [:div {:id "sidebar"}
        (render-configuration state input-signal)]
       (render-exercise question answer clef)
       (render-keyboard state input-signal)]))