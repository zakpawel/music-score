(ns music-score.abc)

(defonce *instrument* (js/Instrument. #js {:wave "piano" :detune 0}))

(defn midi-to-abc [midi]
  (Instrument.midiToPitch midi))

(defn midis-to-abc [midis]
  (->> midis
       (map midi-to-abc)
       (into [])))

(defn render-abc [abc id]
  (let [node (. js/document getElementById id)]
    (js/ABCJS.renderAbc node abc #js {} #js {:add_classes true})))

(defn play-abc [abc]
  (let [inst *instrument*]
    (.play inst abc)))

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
        (map midi-to-abc _)
        (apply str _)
        (parse-abc-file _)
        (extract-freq _)))