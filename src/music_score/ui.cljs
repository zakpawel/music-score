(ns music-score.ui
    (:require [cljs.core.async :as async]
              [clojure.data :as data]
              [music-score.abc :as abc]
              [rum])
    (:require-macros
      [music-score.utils :refer [debug]]
      [cljs.core.async.macros :as async]
      [rum :refer [defc defcs]]))

(enable-console-print!)

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


(defn update-state [state [event [x y]]]
  (print state event x y)
  (let [ret (condp = event
              :drag-start (-> state
                              (assoc :dragging true)
                              (assoc :start-x x)
                              (assoc :start-y y)
                              (assoc :start-value (state :value)))
              :mouse-move (if (state :dragging)
                            (-> state
                                (update :value #(+ (state :start-value) (- x (state :start-x)))))
                            state)
              :drag-end (assoc state :dragging false))]
    (->> ret (clj->js) (.log js/console "update-state"))
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
      (let [initial-state {:dragging false}
            state-atom (atom initial-state)
            ch (async/chan (async/sliding-buffer 1))
            out-ch (foldp update-state initial-state ch)]
        (async/go-loop []
          (let [new-state (<! out-ch)]
            (reset! state-atom new-state)
            (rum/request-render (:rum/react-component state))
            (recur)))
        (-> state
            (assoc ::internal-state state-atom)
            (assoc ::drag-chan ch))))
   :transfer-state
    (fn [old-state state]
      (merge state (select-keys old-state [::internal-state ::drag-chan])))
   :did-update
	 	(fn [state]
      (let [xxx (->> state
                     (::internal-state)
                     (deref)
                     (:value))
            abc (abc/midi-to-abc-string [xxx] :treble)]
        (print "abc is: " abc)
        (abc/render-abc abc "placeholder"))
      #_(->> state (clj->js) (.log js/console "did-update"))
      state
	 		#_(let [input (-> state
				 						(:rum/react-component)
				 						(.getDOMNode))
            local (-> state (::internal-state))
            value (-> state (:rum/args) (first) (:value))]
				(aset input "value" (str value))))}
	[state {:keys [on-blur value]}]
    (let [internal (::internal-state state)
          val (if internal
                (->> @internal (:value) (str))
                0)
          drag-ch (::drag-chan state)]
      #_(print "number-field value==" value val)
      (if internal
        (->> @internal (clj->js) (.log js/console "number-field render"))
        (.log js/console "number-field render internal==null"))
      [:label.number-field {#_:on-blur #_on-blur
               :on-mouse-down (fn [e] (async/put! drag-ch [:drag-start [(.. e -clientX) (.. e -clientY)]])  nil)
               :on-mouse-up (fn [e] (async/put! drag-ch [:drag-end [(.. e -clientX) (.. e -clientY)]])  nil)
               :on-mouse-move (fn [e] (async/put! drag-ch [:mouse-move [(.. e -clientX) (.. e -clientY)]])  nil)
               #_:on-mouse-out #_(fn [_] (swap! internal assoc :dragging false) (print internal))} val]))

(defn render-key [n oct-num type channel]
  (let [value (+ (+ n 12) (* oct-num 12))]
    (if (= type :white)
    	[:div  {:class "key white"
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

(defn render-range-config [low-note high-note channel]
  [:div.range-config
   (number-field {:value low-note  :on-blur #(async/put! channel [:config :range 0 (parse-pitch %)])})
   (number-field {:value high-note :on-blur #(async/put! channel [:config :range 1 (parse-pitch %)])})
   [:div#placeholder]]
  )

(defn render-configuration [model channel]
  (let [clef (get-in model [:config :key])
        switch {true "selected-clef" false ""}
        low-note (-> (get-in model [:config :range 0]) str)
        high-note (-> (get-in model [:config :range 1]) str)]
        [:div
          [:div {:class (switch (= :bass clef))   :on-click #(async/put! channel [:config :key :bass])} "K:bass"]
          [:div {:class (switch (= :treble clef)) :on-click #(async/put! channel [:config :key :treble])} "K:treble"]
          (render-range-config low-note high-note channel)]
    ))
