(ns music-score.core
    (:require [cljs.core.async :as async]
              [clojure.data :as data]
              [rum]
              [music-score.ui :as ui])
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
(defonce *input-chan* (async/chan))
(defonce *model-chan* (async/chan))
(defonce *instrument* (js/Instrument. #js {:wave "piano" :detune 0}))

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

(defn midi-to-pitch [midi]
  (Instrument.midiToPitch midi))

(defn midis-to-abc [midis]
  (->> midis
    (map midi-to-pitch)
    (into [])))

(defn render-abc [abc id]
  (let [node (. js/document getElementById id)]
    (js/ABCJS.renderAbc node abc #js {} #js {:add_classes true})))

(defn play-abc [abc]
  (let [inst *instrument*]
    (.play inst abc)))

(defn start-game [model]
  (let [min (get-in model [:config :range 0])
        max (get-in model [:config :range 1])]
    (as-> model _
      (assoc-in _ [:question] (random-midis 15 min max))
      (assoc-in _ [:answer] [])
      (assoc-in _ [:stage] :guessing)))
)

(defn get-localstorage-if-exist [window]
  (aget window "localStorage"))

(defn load-config-from-localstorage []
  (print "load-config" js/window)
  (if-let [ls (get-localstorage-if-exist js/window)]
    (.setItem ls "__music_score_config" "it works!")
    (print (.getItem ls "__music_score_config"))))

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
  (print event-type args)
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
      (ui/render-configuration data *input-chan*)]
    [:div {:id "notation-exercise"}]
    [:div {:id "notation-answer"}]
    (ui/render-keyboard data *input-chan*)])

(defn render-app []
  (rum/mount (root-component @*app-state*) (. js/document (getElementById "app"))))

(async/go
  (loop []
    (let [event (<! *input-chan*)]
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

(async/go
  (loop []
    (let [model (<! *model-chan*)
          clef (get-in model [:config :key])
          abc (map midi-to-pitch (model :answer))
          abc-str (add-clef clef (apply str abc))
          last-key (str (last abc))
          abc-question-str (add-clef clef (apply str (midis-to-abc (model :question))))]
      (load-config-from-localstorage)
      (render-app)
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

(defn parse-abc-file [abc-str]
  (js/parseABCFile abc-str))

(defn extract-freq [tones]
  (let [stems (-> tones
                (aget "voice")
                (aget "")
                (aget "stems")
              )
        freqs (reduce (fn [acc stem]
                        (conj acc (reduce (fn [acc note]
                                  (conj acc (note "frequency")))
                                #{} (stem "notes"))))
                      [] (js->clj stems))
        ]
    (print freqs)
    freqs))

(defn midi-to-freq [midis]
  (as-> midis _
    (map midi-to-pitch _)
    (apply str _)
    (parse-abc-file _)
    (extract-freq _)))

(defn request-web-midi []
  (let [channel (async/chan)
        midi-access-promise (.requestMIDIAccess js/navigator)
        midi-access (.then midi-access-promise #(async/put! channel %))]
    channel))

(defn ^:export main []
  ;; kickoff
  (async/go
  ;;  (<! (async/timeout 5000))
    (>! *input-chan* [:load-config]))


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
                (async/put! *input-chan* [:key-press pitch])))))))))
(main)