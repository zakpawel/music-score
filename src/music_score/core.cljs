(ns music-score.core
  (:require [cljs.core.async :as async]
            [clojure.data :as data]
            [cljs.pprint :as pprint]
            [rum]
            [datascript.core :as d]
            [cljsjs.d3]
            [music-score.ui :as ui]
            [music-score.abc :as abc]
            [music-score.vex :as vex]
            [jamesmacaulay.zelkova.signal :as z])
  (:require-macros [cljs.core.async.macros :as async]))

(enable-console-print!)

(println "Edits to this text should show up in your developer console.")

;; play with d3

(def id (let [huhu (atom 0)]
          (fn [] (swap! huhu inc)
            @huhu)))

(defn generate-circle [width height]
  #js {:id   (id)
       :r    (+ 10 (rand-int 20))
       :x    (rand-int width)
       :y    (rand-int height)
       :fill (str "rgb(" (rand-int 256) "," (rand-int 256) "," (rand-int 256) ")")})

(declare render-d3)

(let [w 300
      h 150
      svg (-> js/d3
              (.select "body")
              (.append "svg")
              (.attr "id" "dots"))]
  (async/go-loop [data #js []]
                 (.push data (generate-circle w h))
                 (when (> (.. data -length) 3)
                   (.shift data)
                   )
                 (async/<! (async/timeout 1000))
                 (render-d3 svg data)
                 (recur data)))


(defn render-d3 [svg data]
  (let [c (-> svg
              (.selectAll "circle")
              (.data data (fn [d] (.-id d))))]
    (-> c
        (.enter)
        (.append "circle")
        (.attr "opacity" 0)
        (.attr "r" 0)
        (.attr "cx" (fn [d] (.-x d)))
        (.attr "cy" (fn [d] (.-y d)))
        (.attr "fill" (fn [d] (.-fill d)))
        (.transition)
        (.attr "r" (fn [d] (.-r d)))
        (.attr "opacity" 1))

    (-> c
        (.exit)
        (.transition)
        (.attr "opacity" 0)
        (.attr "r" 0))))


;; lets play with datomic

(def schema
  {:mouse/dragging     {}
   :config/clef        {}
   :config/range       {}
   :config/notes-count {}
   :game/config        {:db/valueType :db.type/ref :db/cardinality :db.cardinality/one}
   :game/stage         {}
   :game/current-exercise {}
   :game/exercises      {:db/valueType :db.type/ref :db/cardinality :db.cardinality/many}
   :exercise/from      {}
   :exercise/to        {}
   :exercise/clef      {}
   :exercise/guesses     {:db/valueType :db.type/ref :db/cardinality :db.cardinality/many}
   :guess/question     {}
   :guess/answer       {}})


(def mk-id
  (let [id (atom 0)]
    (fn []
      (swap! id dec)
      @id)))

(def mk-guess
  (fn mk-guess
    ([q a]
     (as-> {:guess/question  q
            :guess/answer    a
            :db/id           (mk-id)} _
           (if a
             _
             (dissoc _ :guess/answer))))
    ([q] (mk-guess q nil))))

(def random-int
  (fn [from to]
    (-> to
         (- from)
         (inc)
         (rand-int)
         (+ from))))


(def mk-exercise
  (fn [from to clef n]
    {:db/id              (mk-id)
     :exercise/from      from
     :exercise/to        to
     :exercise/clef      clef
     :exercise/guesses     (->> (range 0 n)
                              (map #(mk-guess (random-int from to) nil #_(random-int from to)))
                              (into []))}))


(def conn (let [conn (d/create-conn schema)
                records (->> (range 1)
                             (map #(mk-exercise 50 70 :bass 5))
                             (into []))
                game {:db/id         (mk-id)
                      #_:game/exercises #_records
                      :game/stage    :guessing
                      :game/config   {:db/id        (mk-id)
                                      :config/clef  :treble
                                      :config/range [50 70]
                                      :config/notes-count 5}}
                new-db (d/db-with @conn [game])]
            (reset! conn new-db)
            (pprint/pprint new-db)
            #_(print (->> game
                        (map :game/exercises)
                        (map :exercise/guesses)
                        #_((fn [a] (print a) a))
                        (map (fn [s]
                               (->> s
                                    (map #(vector (% :db/id)
                                                  (% :guess/question)
                                                  (% :guess/answer)) s)
                                    (filter (fn [[id q a]] (= q a))))))))

            #_(d/transact! conn [game])
            #_(d/transact! conn [{:exercise/guesses [(-> (mk-guess (random-int 10 15) (random-int 10 15))
                                                       (assoc :db/id 2)
                                                       ((fn [a] (print a) a))
                                                       )]}])
            (let [result (d/q '[:find [(pull ?g [:guess/question]) ...]
                                :in $
                                :where
                                [?e :exercise/guesses ?g]
                                [?g :guess/question ?q]
                                [(get-else $ ?g :guess/answer nil) ?a]
                                ] @conn)]
              (->> result (pprint/pprint)))
            (let [[game-id config-id clef [min max] n]
                  (d/q '[:find [?g ?c ?clef ?r ?n]
                         :where
                            [?g :game/config ?c]
                            [?c :config/clef ?clef]
                            [?c :config/range ?r]
                            [?c :config/notes-count ?n]] @conn)
                  new-db (d/db-with @conn [{:db/id game-id
                                            :game/exercises [(mk-exercise min max clef n)]
                                            :game/stage :guessing}])]
              (println "query config" config-id clef min max)
              (pprint/pprint new-db)
              )
            conn
            ))

;; with-db tx-data
;; tx-data [ [:db.fn/call select-room room-id]
;;           []
;;           [] ]


;; datascript issues:
;; problem with counting (count ?q) (count ?a)
;; how to return two collections qs as



(defonce initial-model
        {:question [] #_(->> (range)
                             (map #(+ 48 %))
                             (take 54)
                             (into []))
         :answer   []
         :stage    :guessing
         :dragging false
         :config   {:key   :treble
                    :range [53 88]}})

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
  (let [[game-id config-id clef [min max] n]
        (d/q '[:find [?g ?c ?clef ?r ?n]
               :where
               [?g :game/config ?c]
               [?c :config/clef ?clef]
               [?c :config/range ?r]
               [?c :config/notes-count ?n]] model)
        x (println "start-game" game-id clef min max n)
        new-exercise (mk-exercise min max clef n)
        report (d/with model [{:db/id game-id
                                  :game/exercises [new-exercise]
                                  :game/stage :guessing
                                  :mouse/dragging false}])
        new-exercise-id
        (-> report
             (:tempids)
             (get (:db/id new-exercise)))

        new-db (d/db-with (:db-after report) [{:db/id game-id
                                               :game/current-exercise new-exercise-id}])]
    #_(pprint/pprint new-db)
    new-db)
  )

(defn key-press [model midi]
 (print "key-press handler")

  (let [[game-id stage ex-id]
        (d/q '[:find [?game ?s ?e]
               :where [?game :game/current-exercise ?e]
                      [?game :game/stage ?s]
                      [?e :exercise/guesses ?g]] model)]

    (condp = stage
      :finished (start-game model)
      :guessing
      (let [guess-id (d/q '[:find ?g .
                            :in $ ?ex
                            :where [?ex :exercise/guesses ?g]
                            [(get-else $ ?g :guess/answer nil) ?a]
                            [(nil? ?a)]] model ex-id)

            new-db (-> model
                (d/db-with
                  [{:db/id ex-id :exercise/guesses [{:db/id        guess-id
                                                   :guess/answer midi}]}]))

            qn
            (d/q '[:find [(count ?g1)]
                   :in $ ?gg
                   :where
                   [?gg :game/current-exercise ?e1]
                   [?e1 :exercise/guesses ?g1]
                   [?g1 :guess/question ?q]] new-db game-id)

            an
            (d/q '[:find [(count ?g2)]
                   :in $ ?gg
                   :where
                   [?gg :game/current-exercise ?e2]
                   [?e2 :exercise/guesses ?g2]
                   [?g2 :guess/answer ?a]] new-db game-id)


            ]
        new-db
        (print "key-press" qn an)

        (if (= qn an)
          (d/db-with new-db [[:db/add game-id :game/stage :finished]])
          new-db))

      )
    )
 )


(defn config [model [config-type & args]]
  (let [[config-id crange] (d/q '[:find [?c ?r]
                         :where
                         [?g :game/config ?c]
                         [?c :config/range ?r]] model)
        x (print "config" config-id crange)

        m (condp = config-type
            :key
            (let [[clef] args]
              (d/db-with model [[:db/add config-id :config/clef clef]])) #_(assoc-in model [:config :key] (first args))

            :range
            (let [[bound value] args]
              (condp = bound
                0
                (d/db-with model [[:db/add config-id :config/range (assoc crange 0 value)]])
                1
                (d/db-with model [[:db/add config-id :config/range (assoc crange 1 value)]])
                )) #_(assoc-in model [:config :range (first args)] (second args))
            model)]
    (println m)
    m))


(defn handle-drag [model dragging?]
  (let [game-id (d/q '[:find ?g .
                       :where [?g :game/config _]] model)]
    (d/db-with model [[:db/add game-id :mouse/dragging dragging?]]))
  #_(assoc model :dragging dragging?))


(defn update-model [[event-type & args] model]
  (println "update-model" event-type args)
  (let [new-model (condp = event-type
                    :start-game (start-game model)
                    :key-press (key-press model (first args))
                    :config (config model args)
                    :dragging (handle-drag model (first args))
                    model
                    )]
    (println new-model)
    new-model))

;; here foldp
(defonce state-signal
         (z/foldp update-model (start-game @conn) input-signal))

(defn convert-cfg [cfg]
  {:key (:config/clef cfg)
   :range (:config/range cfg)})

;; render function

(defn render-app [db]
  (println "render-app")
  (let [x (d/q '[:find ?g ?q ?a
                 :where [?game :game/stage ?s]
                       [?game :game/current-exercise ?e]
                       [?e :exercise/guesses ?g]
                       [?g :guess/question ?q]
                       [(get-else $ ?g :guess/answer nil) ?a]] db)
        [qs as] (reduce (fn [[qs as qn an] [id q a]]
                  [(conj qs q) (if a (conj as a) as) (inc qn) (inc an)]) [[] []] x)
        ccc (println x qs as)
        [dragging config] (d/q '[:find [?d (pull ?c [*])]
                                     :where
                                     [?g :mouse/dragging ?d]
                                     [?g :game/config ?c]] db)
         state {:question qs
                :answer   as
                :dragging dragging
                :config   (convert-cfg config)
                }]
    (println "render-app" state)
    (rum/mount (ui/root-component state input-signal) (. js/document (getElementById "app")))))

(defonce html-signal (z/map render-app state-signal))

;; kicks everything off
(defonce app-state (z/pipe-to-atom html-signal))


(defn on-js-reload []
  ;; optionally touch your *app-state* to force rerendering depending on
  ;; your application
  ;; (swap! *app-state* update-in [:__figwheel_counter] inc)
  )


(defn request-web-midi []
 (let [channel (async/chan)
       midi-access-promise (.requestMIDIAccess js/navigator)
       midi-access (.then midi-access-promise #(async/put! channel %))]
   channel))


(defn ^:export main []
  (println "main called")
  (let [ch (z/to-chan input-signal)]
    (async/go-loop
      []
      (let [[evt midi] (<! ch)]
        (when (= evt :key-press)
          (print "play!!!!")
          (abc/play-abc (abc/midi-to-abc midi)))
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
             ;; (js/console.log "onmidimessage" event)
             (let [data (aget event "data")
                   type (-> (aget data 0) (bit-and 0xF0))
                   pitch (aget data 1)]
               (when (= type 144)
                 (js/console.log type pitch)
                 (async/put! input-signal [:key-press pitch]))))))))

(main)