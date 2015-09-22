(ns music-score.ui
    (:require [cljs.core.async :as async]
              [clojure.data :as data]
              [rum])
    (:require-macros [cljs.core.async.macros :as async]
    				 [rum :refer [defc]]))

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

(defc number-field < 
	{:did-update
	 	(fn [state]
	 		(let [input (-> state
				 						(:rum/react-component)
				 						(.getDOMNode))]
				(aset input "value" (-> state (:rum/args) (first) (:value)))))}
	[model]
	[:input {:on-blur (:on-blur model)}])

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

(defn render-configuration [model channel]
  (let [clef (get-in model [:config :key])
        switch {true "selected-clef" false ""}
        low-note (-> (get-in model [:config :range 0]) str)
        high-note (-> (get-in model [:config :range 1]) str)]
        [:div
          [:div {:class (switch (= :bass clef))   :on-click #(async/put! channel [:config :key :bass])} "K:bass"]
          [:div {:class (switch (= :treble clef)) :on-click #(async/put! channel [:config :key :treble])} "K:treble"]
          (number-field {:value low-note  :on-blur #(async/put! channel [:config :range 0 (parse-pitch %)])})
          (number-field {:value high-note :on-blur #(async/put! channel [:config :range 1 (parse-pitch %)])})]
    ))
