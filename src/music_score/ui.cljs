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

(defonce drag-channel (z/write-port [:nothing [0 0]])#_(async/chan (async/sliding-buffer 1)))

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


(defn get-evt-coord [e]
  [(.. e -clientX) (.. e -clientY)])

(defn update-state [state [event [x y]]]
  (let [ret (condp = event
              :drag-start (do (print "mousedown received") (-> state
                                                      (assoc :dragging true)
                                                      (assoc :start-x x)
                                                      (assoc :start-y y)
                                                      (assoc :start-value (state :value))))
              :mouse-move (if (state :dragging)
                            (-> state
                                (update :value #(+ (state :start-value) (- x (state :start-x)))))
                            state)
              :drag-end (do (print "mouseup received") (assoc state :dragging false)))]
    (->> ret (clj->js) (.log js/console "update-state" (str event)))
    ret))

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
          (let [value (-> state (:rum/args) (first) (:value))
                initial-state {:dragging false :value value}
                ch (z/write-port [:nothing [0 0]])
                zzz (z/merge ch drag-channel)
                out-ch (foldp update-state initial-state (z/to-chan zzz))
                out-fn (->> state
                            (:rum/args)
                            (second))]
            (async/go-loop [old-state initial-state]
                           (let [new-state (<! out-ch)
                                 old-value (:value old-state)
                                 new-value (:value new-state)]
                             (when (not= old-value new-value)
                               (out-fn new-value))
                             (recur new-state)))
            (-> state
                (assoc ::drag-chan ch))))
        :transfer-state
        (fn [old-state state]
          (merge state (select-keys old-state [::drag-chan])))}
       [state {:keys [on-blur value]} channel]
       (let [drag-ch (::drag-chan state)]
         [:div.number-field
          {:on-mouse-down (fn [e] (print "mousedown" value) (async/put! drag-ch [:drag-start (get-evt-coord e)]) nil)
           #_:on-mouse-up #_(fn [e] (async/put! drag-ch [:drag-end (get-evt-coord e)]) nil)
           #_:on-mouse-move #_(fn [e] (async/put! drag-channel [:mouse-move (get-evt-coord e)]) nil)}
          (abc/midi-to-abc value)]))

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

(defn render-range-config [low-note high-note clef channel]
  [:div.range-config.cell
   (number-field {:value low-note :on-blur #(async/put! channel [:config :range 0 (parse-pitch %)])}
                 #(async/put! channel [:config :range 0 %]))
   (number-field {:value high-note :on-blur #(async/put! channel [:config :range 1 (parse-pitch %)])}
                 #(async/put! channel [:config :range 1 %]))
   (render-notes [low-note high-note] clef "config")]
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
  (let [[f & r] (data/diff question answer)
        full (take (count question) (concat f (repeat nil)))]
    (as-> full _
          (map #(if (nil? %) "correct" "error") _)
          (into [] _)))
  )

(defn query-dom-notes! []
  (let [answer-node (. js/document (getElementById "answer"))
        dom-notes (. answer-node (getElementsByClassName "note"))]
    dom-notes
    )
  )

(extend-type js/HTMLCollection
  ISeqable
  (-seq [array] (array-seq array 0)))

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
     (let [question (-> state (:rum/args) (first))
           answer (-> state (:rum/args) (second))]
       (->> (query-dom-notes!)
            (apply-classes! (check-correctness question answer))))
     state)}
  [question answer clef]
    [:div
      (render-notes question clef "question")
      (render-notes answer clef "answer")])

(defc render-notes <
      {:should-update
       (fn [old-state new-state]
         (let [neq (not= (:rum/args old-state) (:rum/args new-state))]
           (print "render-notes should-update" neq (-> old-state (:rum/args)) (-> new-state (:rum/args)))
           neq))
       :did-update
       (fn [state]
         (let [[midi clef] (-> state (:rum/args))
               node (-> state
                        (:rum/react-component)
                        (.getDOMNode))
               abc (abc/midi-to-abc-string midi clef)]
           (print "render-notes did-update" (-> state (:rum/args)))
           (abc/render-abc abc node)
           state))}
  [midi clef id]
  [:div {:id id}])

(rum/defc root-component
  [state input-signal]
    (let [clef (get-in state [:config :key])
          question (state :question)
          answer (state :answer)]
      [:div {:id            "container"
             :on-mouse-move #(do (async/put! drag-channel [:mouse-move (get-evt-coord %)]) nil)
             :on-mouse-up   #(do (print "onmouseup") (async/put! drag-channel [:drag-end (get-evt-coord %)]) nil)
             :on-mouse-leave #(do (print "onmouseleave") (async/put! drag-channel [:drag-end (get-evt-coord %)]) nil)}
       [:div {:id "sidebar"}
        (render-configuration state input-signal)]
       (render-exercise question answer clef)
       (render-keyboard state input-signal)]))
