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

;; define your app data so that it doesn't get over-written on reload

(defonce *app-state*
  (atom {:question []
         :answer []
         :stage :guessing
         :config {:key :bass
                  :range []}}))

(defonce initial-model
         {:question []
          :answer []
          :stage :guessing
          :config {:key :bass
                   :range []}})

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


(rum/defc root-component [data]
  [:div {:id "container"}
    [:div {:id "sidebar"}
      (ui/render-configuration data input-signal)]
    [:div {:id "notation-exercise"}]
    [:div {:id "notation-answer"}]
    (ui/render-keyboard data input-signal)])

;; here foldp

(defonce state-signal
         (z/foldp update-model initial-model input-signal))

#_(async/go
  (loop []
    (let [event (<! input-signal)]
      (swap! *app-state* (partial update-model event))
      (>! *model-chan* @*app-state*)
    (recur))))

(defn check-correctness [question answer]
  (let [[f & r] (data/diff question answer)
        full (take (count question) (concat f (repeat nil)))]
    (as-> full _
        (map #(if (nil? %) "correct" "error") _)
        (into [] _)))
)

(defn query-dom-notes []
  (let [answer-node (. js/document (getElementById "notation-answer"))
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

(defn add-clef [clef abc-str]
  (if (= clef :bass)
    (str "K:bass\n" abc-str)
    abc-str))


(defn render-app [state]
  (print "render-app")
  (let [model state
        clef (get-in model [:config :key])
        abc (map abc/midi-to-abc (model :answer))
        abc-str (add-clef clef (apply str abc))
        last-key (str (last abc))
        abc-question-str (add-clef clef (apply str (abc/midis-to-abc (model :question))))]
    ;; (load-config-from-localstorage)
    ;; (render-app model)
    (rum/mount (root-component state) (. js/document (getElementById "app")))
    (abc/render-abc abc-str "notation-answer")
    ;; (play-abc last-key)
    (apply-classes! (check-correctness (model :question) (model :answer)) (query-dom-notes))
    (abc/render-abc abc-question-str "notation-exercise")))



(defonce main-signal (z/map render-app state-signal))


;; kicks everything off
(defonce app-state (z/pipe-to-atom main-signal))

#_(async/go
  (loop []
    (let [model (<! *model-chan*)
          clef (get-in model [:config :key])
          abc (map midi-to-abc (model :answer))
          abc-str (add-clef clef (apply str abc))
          last-key (str (last abc))
          abc-question-str (add-clef clef (apply str (midis-to-abc (model :question))))]
      (load-config-from-localstorage)
      (render-app model)
      (render-abc abc-str "notation-answer")
      (play-abc last-key)
      (apply-classes! (check-correctness (model :question) (model :answer)) (query-dom-notes))
      (render-abc abc-question-str "notation-exercise")
      (recur))))


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