(ns music-score.core
    (:require [cljs.core.async :as async]
              [clojure.data :as data]
              [rum]
              [music-score.ui :as ui]
              [music-score.abc :as abc]
              [jamesmacaulay.zelkova.signal :as z])
    (:require-macros [cljs.core.async.macros :as async]))

(enable-console-print!)

(println "Edits to this text should show up in your developer console.")


(comment
  :initial :load-config -----> :configure -----> :guessing ----> :success
                             |
                              ----> :mistake
  :stage [:menu :guessing :success :mistake])

(defonce initial-model
         {:question []
          :answer []
          :stage :guessing
          :config {:key :bass
                   :range [20 30]}})

;; (defonce _input-chan (z/write-port 1))
;; (defonce input-signal (z/to-chan (z/write-port 1)))
;; (defonce _input-mult (async/mult _input-chan))
;; (defonce input-signal (async/tap _input-mult (async/chan)))
;; (defonce *model-chan* (async/chan))

(defonce input-signal (z/write-port [:key-press 70]))

(defn random-midis
  ([n] (random-midis n 0 (- 108 21)))
  ([n min] (random-midis n min (- 108 21)))
  ([n min max]
    (->> 
      (range)
      (map #(rand-int (- max min)))
      (map #(+ % min))
      ;;(filter odd?)
      (take n)
      (into []))))

(defn start-game [model]
  (let [min (get-in model [:config :range 0])
        max (get-in model [:config :range 1])]
    (as-> model _
      (assoc-in _ [:question] (random-midis 15 min max))
      (assoc-in _ [:answer] [])
      (assoc-in _ [:stage] :guessing)))
)

(defn key-press [model midi]
  (print "key-press handler")
  (condp = (model :stage)
    :success (start-game model)
    :mistake (start-game model)
    :guessing
      (let [model' (update-in model [:answer] conj midi)
            answer (model' :answer)
            question (model' :question)]
        (if (= answer question)
          (assoc-in model' [:stage] :success)
          (let [ans-count (count answer)
                question-so-far (take ans-count (model' :question))]
            (if (= answer question-so-far)
              model'
              (assoc-in model' [:stage] :mistake))
          )
        )
      )
  )
)

(defn config [model [config-type & args]]
  (let [m (condp = config-type
            :key (assoc-in model [:config :key] (first args))
            :range (assoc-in model [:config :range (first args)] (second args))
            model)]
  (println m)
  m))

(defn update-model [[event-type & args] model]
  (print "update-model" event-type args)
  (condp = event-type
    :start-game (start-game model)
    :key-press (key-press model (first args))
    :config (config model args)
    model
  ))

(defn model-to-pitch-range-str [model]
  (let [min (get-in model [:config :range 0])
        max (get-in model [:config :range 1])]
    (str min " " max)
  ))


(rum/defc root-component [state]
  (let [clef (get-in state [:config :key])
        question (state :question)
        answer (state :answer)]
    [:div {:id "container"}
     [:div {:id "sidebar"}
      (ui/render-configuration state input-signal)]
     (ui/render-exercise question answer clef)
     (ui/render-keyboard state input-signal)]))

;; here foldp

(defonce state-signal
         (z/foldp update-model initial-model input-signal))


(defn render-app [state]
  (let [clef (get-in state [:config :key])
        question (state :question)
        answer (state :answer)
        abc-question (abc/midi-to-abc-string question clef)
        abc-answer (abc/midi-to-abc-string answer clef)]
    (print "render-app" abc-question)
    (rum/mount (root-component state) (. js/document (getElementById "app")))
    #_(abc/render-abc-id abc-answer "notation-answer")
    #_(->> (query-dom-notes!)
         (apply-classes! (check-correctness question answer)))
    #_(abc/render-abc-id abc-question "notation-exercise")))



(defonce main-signal (z/map render-app state-signal))


;; kicks everything off
(defonce app-state (z/pipe-to-atom main-signal))


(defn on-js-reload []
  ;; optionally touch your *app-state* to force rerendering depending on
  ;; your application
  ;; (swap! *app-state* update-in [:__figwheel_counter] inc)
)

(def octave "CDEFGAB")

(defn raise-octave [n]
  (->> octave
    (map (fn [pitch]
      (if (pos? n)
                (apply str (conj (repeat n "'") pitch))
                (str (apply str (repeat (- 0 n) ",")) pitch))))
    (into [])))



(defn request-web-midi []
  (let [channel (async/chan)
        midi-access-promise (.requestMIDIAccess js/navigator)
        midi-access (.then midi-access-promise #(async/put! channel %))]
    channel))

(defn ^:export main []
  (let [ch (z/to-chan input-signal)]
    (async/go-loop
      []
      (let [[keypress midi] (<! ch)]
        (print "play!!!!")
        (abc/play-abc (abc/midi-to-abc midi))
        (recur)))))


  ;; try midi
  (async/go
    (let [midi-access (<! (request-web-midi))
          device (as-> midi-access _
                  (aget _ "inputs")
;;                  (.-inputs _)
                  (.values _)
                  (.next _)
;;                  (.-value _)
                  (aget _ "value")
                  )]
  ;;    (js/console.log device)
      (when device
        (aset device "onmidimessage"
          (fn [event]
    ;;        (js/console.log event)
            (let [data (aget event "data")
                  type (-> (aget data 0) (bit-and 0xF0))
                  pitch (aget data 1)]
              (when (= type 144)
                (js/console.log type pitch)
                (async/put! input-signal [:key-press pitch]))))))))
(main)