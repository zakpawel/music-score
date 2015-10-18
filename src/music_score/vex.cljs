(ns music-score.vex)

;   // use StaveNote.setStyle() to color all noteheads & the stem:
;   var C7 = new Vex.Flow.StaveNote({ keys: ['C/4', 'E/4', 'G/4', 'Bb/4'], duration: '8'});
;   C7.setStyle({strokeStyle: "blue", stemStyle: "blue"});
;
;   // use StaveNote.setKeyStyle(keyIndex, styleObject) to style an individual notehead.
;   // in this example, we use keyIndex = 2, referring to the key "A/4"
;   var FMaj = new Vex.Flow.StaveNote({ keys: ['C/4', 'F/4', 'A/4'], duration: '8'});
;   FMaj.setKeyStyle(2, {shadowColor: "yellow", shadowBlur: 3});

;; 1-12
(def num-to-note
  {1  [{:pitch :C} {:pitch :C}]
   2  [{:pitch :C :acc :#} {:pitch :D :acc :b}]
   3  [{:pitch :D} {:pitch :D}]
   4  [{:pitch :D :acc :#} {:pitch :E :acc :b}]
   5  [{:pitch :E} {:pitch :E}]
   6  [{:pitch :F} {:pitch :F}]
   7  [{:pitch :F :acc :#} {:pitch :G :acc :b}]
   8  [{:pitch :G} {:pitch :G}]
   9  [{:pitch :G :acc :#} {:pitch :A :acc :b}]
   10 [{:pitch :A} {:pitch :A}]
   11 [{:pitch :A :acc :#} {:pitch :B :acc :b}]
   12 [{:pitch :B} {:pitch :B}]})

(defn midi-to-my-note [midi]
  ;; range is <21 108>
  ;;           C0 C8
  (when midi
    (print "midi-to-my-note" midi)
    (let [midi-norm (- midi 12)
          octave-n (-> midi-norm (/ 12) (int))
          num (-> midi
                  (mod 12)
                  (inc)
                  (num-to-note)
                  (nth 0 #_(rand-int 2)))]
      (assoc num :octave octave-n)))
  )

(defn my-note-to-str [my-note]
  (print "my-note-to-str" my-note)
  (when my-note
    (let [{:keys [pitch acc octave]} my-note
          my-name (fnil name "")]
        (vector (str (my-name pitch) (my-name acc) "/" octave) (my-name acc)))))


(defn stave-note [my-note duration clef]
  (let [[idx cls keys acc] my-note
        note (Vex.Flow.StaveNote. #js {:clef (name clef)
                                       :keys     (clj->js keys)
                                       :duration duration})]
    (do
      (print "stave-note" note)
      #_(doseq [[idx k] (map-indexed vector keys)]
        (.setKeyStyle note idx #js {:fillStyle cls}))
      ;; for the moment we assume we will always receive at least two notes (q,a)
      (when (seq acc)
        (doseq [[idx a] (map-indexed vector acc)]
          (when (not (empty? a))
            (.addAccidental note idx (Vex.Flow.Accidental. a)))))
      (.setKeyStyle note idx #js {:fillStyle cls}))
    note))


(defn to-stave-notes [guesses clef]
  (print guesses)
  (let [notes (map (fn [guess]
                   (print "xxx map" guess)
                   (let [[q a] guess
                         [[k1 acc1] [k2 acc2]] (map (comp my-note-to-str midi-to-my-note) guess)
                         cls (condp = a
                                   q [0 "green" [k1] [acc1]]
                                   nil [0 "" [k1] [acc1]]
                                   (if (< a q)
                                     [0 "red" [k2 k1] [acc2 acc1]]
                                     [1 "red" [k1 k2] [acc1 acc2]])
                                   )]
                     cls)) guesses)]
    (print notes)
    (->> notes
         (map #(stave-note % "q" clef))
         (into [])
         (clj->js))))

(defn mk-voice
  ([notes]
    (mk-voice notes "w"))
  ([notes beat-value]
    (mk-voice notes (count notes) beat-value))
  ([notes num-beats beat-value]
   (-> (Vex.Flow.Voice. #js {:num_beats  num-beats
                             :beat_value beat-value
                             :resolution Vex.Flow.RESOLUTION})
       (.addTickables notes))))

(defn draw-voices
  ([voices width stave ctx]
     (-> (Vex.Flow.Formatter.)
         (.joinVoices voices)
         (.format voices (- width 50)))
     (doseq [v voices]
       (.draw v ctx stave))))

(defn init-stave! [node clef width]
  (.removeChildren (goog.dom.DomHelper.) node)
  (let [renderer (Vex.Flow.Renderer. node Vex.Flow.Renderer.Backends.RAPHAEL)
        ctx (.getContext renderer)
        stave (Vex.Flow.Stave. 10 100 width)]
    (.addClef stave (name clef))
    (.setContext stave ctx)
    (.draw stave)
    [stave ctx]))

;var text = new Vex.Flow.TextNote({
;                                  text: "Render this",
;                                  font: {
;                                         family: "Arial",
;                                         size: 12,
;                                         weight: ""
;                                         },
;                                  duration: 'w'
;                                  })
;.setLine(2)
;.setStave(stave)
;.setJustification(Vex.Flow.TextNote.Justification.LEFT);
(defn to-text-notes [midi-pairs stave]
  (print "to-text-note" (count midi-pairs))
  (->> midi-pairs
       (map (fn [p]
              (let [midi-to-text-note (fn [[q a]]
                                        (let [props #js {:text     (str q a)
                                                         :font     #js {:family "Arial"
                                                                        :size   12
                                                                        :weight ""}
                                                         :duration "q"
                                                         }]
                                          (Vex.Flow.TextNote. props)))
                    text-note (midi-to-text-note p)]
                (.setStave text-note stave)
                (.setLine text-note 10)
                (.setJustification text-note Vex.Flow.TextNote.Justification.LEFT)
                text-note)))
       (into [])
       (clj->js))
  )

(defn render-vex
  ([node midi-pairs clef]
    (render-vex node midi-pairs clef 1000))
  ([node midi-pairs clef width]
   (let [[stave ctx] (init-stave! node clef width)
         voice (mk-voice (to-stave-notes midi-pairs clef) (count midi-pairs) 4)
         voice2 (mk-voice (to-text-notes midi-pairs stave) (count midi-pairs) 4)]
     (draw-voices #js [voice #_voice2] width stave ctx))))

