(ns music-score.core
  (:require [cljs.core.async :as async]
            [clojure.data :as data]
            [cljs.pprint :as pprint :refer [pprint]]
            [rum]
            [datascript.core :as d]
            [cljsjs.d3]
            [music-score.ui :as ui]
            [music-score.abc :as abc]
            [music-score.vex :as vex]
            #_[music-score.d3-playground :as d3]
            [jamesmacaulay.zelkova.signal :as z])
  (:require-macros [cljs.core.async.macros :as async]))

(enable-console-print!)

(println "Edits to this text should show up in your developer console.")


(def schema
  {:game/mouse-dragging   {}
   :config/clef           {}
   :config/range          {}
   :config/notes-count    {}
   :game/config           {:db/valueType :db.type/ref :db/cardinality :db.cardinality/one}
   :game/stage            {}
   :game/current-exercise {:db/valueType :db.type/ref :db/cardinality :db.cardinality/one}
   :game/exercises        {:db/valueType :db.type/ref :db/cardinality :db.cardinality/many}
   :exercise/from         {}
   :exercise/to           {}
   :exercise/clef         {}
   :exercise/guesses      {:db/valueType :db.type/ref :db/cardinality :db.cardinality/many}
   :guess/question        {}
   :guess/answer          {}})


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
    {:db/id            (mk-id)
     :exercise/from    from
     :exercise/to      to
     :exercise/clef    clef
     :exercise/guesses (->> (range 0 n)
                            (map #(mk-guess (random-int from to) nil #_(random-int from to)))
                            (into []))}))


(def conn (let [conn (d/create-conn schema)
                game {:db/id         (mk-id)
                      :game/stage    :guessing
                      :game/config   {:db/id        (mk-id)
                                      :config/clef  :treble
                                      :config/range [50 70]
                                      :config/notes-count 5}}
                new-db (d/db-with @conn [game])]
            (reset! conn new-db)
            (println "init config" new-db)

            ;; testing
            #_(let [result (d/q '[:find (count ?g1)
                                :in $
                                :where
                                [?e :exercise/guesses ?g1]
                                [?e :exercise/guesses ?g2]
                                [?g1 :guess/question ?q1]
                                #_[?g2 :guess/question ?q2]
                                [?g2 :guess/answer ?a]
                                #_[(get-else $ ?g :guess/answer nil) ?a]
                                ] @conn)]
              (->> result (pprint/pprint)))
            conn
            ))


(defonce input-signal (z/write-port [:key-press 70]))

(defn start-game [db]
  (let [
        [game-id clef [min max] n]
        (d/q '[:find [?g ?clef ?r ?n]
               :where
               [?g :game/config ?c]
               [?c :config/clef ?clef]
               [?c :config/range ?r]
               [?c :config/notes-count ?n]] db)
        x (println "start-game" game-id clef min max n)

        new-exercise (mk-exercise min max clef n)

        new-db (d/db-with db [new-exercise
                              {:db/id game-id
                               :game/exercises [new-exercise]
                               :game/current-exercise (:db/id new-exercise)
                               :game/stage :guessing
                               :game/mouse-dragging false}])]
    new-db)
  )

(defn key-press [db midi]
  (print "key-press handler" midi)

  (let [[game-id stage ex-id]
        (d/q '[:find [?game ?s ?e]
               :where
               [?game :game/current-exercise ?e]
               [?game :game/stage ?s]
               [?e :exercise/guesses ?g]] db)]

    (condp = stage
      :finished (start-game db)
      :guessing
      (let [guess-id (d/q '[:find ?g .
                            :in $ ?ex
                            :where
                            [?ex :exercise/guesses ?g]
                            [(get-else $ ?g :guess/answer nil) ?a]
                            [(nil? ?a)]] db ex-id)

            new-db (-> db
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
        (print "key-press handler" qn an)

        (if (= qn an)
          (d/db-with new-db [[:db/add game-id :game/stage :finished]])
          new-db))

      )
    )
 )


(defn config [db [config-type & args]]
  (let [[config-id crange]
        (d/q '[:find [?c ?r]
               :where
               [?g :game/config ?c]
               [?c :config/range ?r]] db)

        x (print "config" config-id crange)

        new-db
        (condp = config-type
            :key
            (let [[clef] args]
              (d/db-with db [[:db/add config-id :config/clef clef]]))

            :range
            (let [[bound value] args]
              (d/db-with db [[:db/add config-id :config/range (assoc crange bound value)]]))
            db)]
    new-db))


(defn handle-drag [db dragging?]
  (let [game-id (d/q '[:find ?g .
                       :where [?g :game/config _]] db)]
    (d/db-with db [[:db/add game-id :game/mouse-dragging dragging?]])))


(defn update-db [[event-type & args] db]
  (println "update-db" event-type args)
  (let [new-db (condp = event-type
                    :start-game (start-game db)
                    :key-press  (key-press db (first args))
                    :config     (config db args)
                    :dragging   (handle-drag db (first args))
                    db
                    )]
    new-db))

;; here foldp
(defonce state-signal
         (z/foldp update-db (start-game @conn) input-signal))

(defn convert-cfg [cfg]
  {:key   (:config/clef cfg)
   :range (:config/range cfg)})

;; render function

(defn render-app [db]
  (println "render-app")
  (let [result (d/q '[:find ?q ?a ?g
                      :where
                      [?game :game/stage ?s]
                      [?game :game/current-exercise ?e]
                      [?e :exercise/guesses ?g]
                      [?g :guess/question ?q]
                      [(get-else $ ?g :guess/answer nil) ?a]] db)
        gs (map butlast result)
        [dragging config] (d/q '[:find [?d (pull ?c [*])]
                                 :where
                                 [?g :game/mouse-dragging ?d]
                                 [?g :game/config ?c]] db)
         state {:guesses  gs
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
                    (aget _ "inputs") ;; (.-inputs _)
                    (.values _)
                    (.next _)
                    (aget _ "value") ;; (.-value _)
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