(ns music-score.core
  (:require [cljs.core.async :as async]
            [clojure.data :as data]
            [cljs.pprint :as pprint :refer [pprint]]
            [datascript.core :as d]
            [cljsjs.d3]
            [music-score.ui :as ui]
            [music-score.abc :as abc]
            [music-score.vex :as vex]
            [music-score.keyboard :as keyboard]
            [music-score.start-app :as app]
            [devtools.core :as devtools]
            [music-score.d3playground :as d3]
            [jamesmacaulay.zelkova.signal :as z]
            [jamesmacaulay.zelkova.keyboard :as zk])
  (:require-macros [cljs.core.async.macros :as async]
                   [music-score.utils :refer [print println]]))

(def debug 0)
#_(enable-console-print!)

#_(devtools/install!)

#_(println "Edits to this text should show up in your developer console.")

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
   :guess/answer          {}
   :game/keyboard         {}})


(def mk-id
  (let [id (atom 0)]
    (fn []
      (swap! id dec)
      @id)))

(def mk-guess
  (fn mk-guess
    ([q a]
     (as-> {:guess/question q
            :guess/answer   a
            :db/id          (mk-id)} _
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


(defonce conn (let [conn (d/create-conn schema)
                    game {:db/id         (mk-id)
                          :game/stage    :guessing
                          :game/config   {:db/id              (mk-id)
                                          :config/clef        :treble
                                          :config/range       [50 70]
                                          :config/notes-count 12}
                          :game/keyboard keyboard/initial-model}
                    new-db (d/db-with @conn [game])]
                (reset! conn new-db)
                (println "init config" new-db)

                ;; testing
                #_(let [result (d/q '[:find ?q1 (count ?q1)
                                      :in $
                                      :where
                                      [?e :exercise/guesses ?g1]
                                      #_[?e :exercise/guesses ?g2]
                                      [?g1 :guess/question ?q1]
                                      #_[?g2 :guess/question ?q2]
                                      #_[?g1 :guess/answer ?a]
                                      #_[(get-else $ ?g :guess/answer nil) ?a]
                                      ] @conn)]
                    (->> result (pprint/pprint)))
                conn
                ))

(defn start-game [db]
  (let [
        [game-id clef [min max] n]
        (d/q '[:find [?g ?clef ?r ?n]
               :where
               [?g :game/config ?c]
               [?c :config/clef ?clef]
               [?c :config/range ?r]
               [?c :config/notes-count ?n]] db)
        x (js/console.log "start-game" game-id clef min max n)

        new-exercise (mk-exercise min max clef n)

        new-db (d/db-with db [new-exercise
                              {:db/id                 game-id
                               :game/exercises        [new-exercise]
                               :game/current-exercise (:db/id new-exercise)
                               :game/stage            :guessing
                               :game/mouse-dragging   false}])]
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
      :finished
      (start-game db)

      :guessing
      (let [guesses (d/q '[:find ?g
                           :in $ ?ex
                           :where
                           [?ex :exercise/guesses ?g]
                           [(get-else $ ?g :guess/answer nil) ?a]
                           [(nil? ?a)]] db ex-id)

            guess-id (->> guesses
                          (sort-by first)
                          (ffirst))

            x (println "key-press guess-id" guesses guess-id)

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
        db
      )
    )
  )

(defn config [db [config-type & args]]
  (let [[config-id crange]
        (d/q '[:find [?c ?r]
               :where
               [?g :game/config ?c]
               [?c :config/range ?r]] db)
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

(defn update-config [db [event-type selection]]
  (let [stage
        (d/q '[:find ?s .
               :where [?g :game/stage ?s]] db)]
    (if (= :configuring stage)
      (let [sorted (sort selection)
            low (first sorted)
            high (last sorted)]
        (-> db
            (config [:range 0 low])
            (config [:range 1 high])))
      db)))

(defn update-keyboard-model [db action]
  (let [[game-id keyboard-model]
        (d/q '[:find [?g ?k]
               :where
               [?g :game/keyboard ?k]] db)

        new-db
        (d/db-with db
                   [[:db/add game-id
                     :game/keyboard
                     (keyboard/update-model action keyboard-model)]])]
    new-db)
  )

(declare input-signal)

(defn keyboard-event [db [[event-type arg :as action]]]
  (let [action-type (namespace event-type)]
    (if (= action-type "keyboard.action")
      (condp = event-type
        :keyboard.action/selection
        (update-config db action)

        :keyboard.action/key-down
        (do
          (async/put! input-signal (app/make-action :core.action/key-press arg))
          (update-keyboard-model db action))

        ;; else
        (update-keyboard-model db action))
      db)))

(defn pc-key [db [key-map]]
  (if (->> (key-map :key-codes)
           (empty?)
           (not))
    (let [[game-id stage]
          (d/q '[:find [?g ?s]
                 :where [?g :game/stage ?s]] db)
          next-stage (if (= stage :guessing)
                       :configuring
                       :guessing)
          new-db
          (d/db-with db [[:db/add
                          game-id
                          :game/stage next-stage]])]
      new-db)
    db))

(defn update-db [[event-type & args] db]
  (let [new-db (condp = event-type
                 :core.action/start-game (start-game db)
                 :core.action/key-press (key-press db (first args))
                 :core.action/config (config db args)
                 :core.action/dragging (handle-drag db (first args))
                 :core.action/keyboard (keyboard-event db args)
                 :core.action/pc-key (pc-key db args)
                 db)]
    new-db))


(defn max-count [db]
  (let [mapper (fn [q a] (if (= q a) 1 0))]
    (d/q '[:find ?q1 (count ?g1) (sum ?a2)
           :in $ ?mapper
           :where
           [?e :exercise/guesses ?g1]
           #_[?e :exercise/guesses ?g2]
           [?g1 :guess/question ?q1]
           #_[?g2 :guess/question ?q2]
           #_[?g1 :guess/answer ?a]
           [(get-else $ ?g1 :guess/answer nil) ?a]
           [(not= nil ?a)]
           [(?mapper ?q1 ?a) ?a2]
           ] db mapper)))

(defn chart-query [db]
  (let [result (max-count db)
        mcount (d/q '[:find (max ?c) .
                      :in [[?q ?c ?s]]] result)
        result2 (d/q '[:find ?q ?c ?s ?m
                       :in ?m [[?q ?c ?s]]] mcount result)
        ]
    result2))


(defn convert-cfg [cfg]
  {:key   (:config/clef cfg)
   :range (:config/range cfg)})

;; render function

(defn render-app [input-signal db]
  (println "render-app")
  (let [result (d/q '[:find ?q ?a ?g
                      :where
                      [?game :game/stage ?s]
                      [?game :game/current-exercise ?e]
                      [?e :exercise/guesses ?g]
                      [?g :guess/question ?q]
                      [(get-else $ ?g :guess/answer nil) ?a]] db)
        gs (->> result
                (sort-by last))
        [dragging? stage config keyboard]
        (d/q '[:find [?d ?s (pull ?cfg [*]) ?k]
               :where
               [?g :game/mouse-dragging ?d]
               [?g :game/stage ?s]
               [?g :game/keyboard ?k]
               [?g :game/config ?cfg]] db)
        state {:guesses      gs
               :dragging?    dragging?
               :config       (convert-cfg config)
               :keyboard     keyboard
               :configuring? (= stage :configuring)
               :chart-data (chart-query db)}
        state (if (= stage :confguring)
                (assoc stage :chart-data (chart-query db))
                state)]
    (ui/root-component input-signal state)))


(defn on-js-reload []
  ;; optionally touch your *app-state* to force rerendering depending on
  ;; your application
  ;; (swap! *app-state* update-in [:__figwheel_counter] inc)
  )



(defonce input-signal (app/start-adv {:init   (start-game @conn)
                                      :update update-db
                                      :view   render-app
                                      :inputs [(app/map-signal #(app/make-action :core.action/keyboard %) keyboard/signal)
                                               (zk/key-signal #(do
                                                                (print "key-signal" %1 %2 %3 %4)
                                                                (app/make-action :core.action/pc-key %)))
                                               #_(app/proxy #(app/make-action :core/keyboard %) keyboard/signal)]}))

#_(defonce input-signal (app/start-adv {:init   keyboard/initial-model
                                      :update keyboard/update-model
                                      :view   keyboard/render-keyboard
                                      :inputs (vector keyboard/signal)}))


#_(defonce input-signal (app/start {:init   keyboard/initial-model
                                    :update keyboard/update-model
                                    :view   keyboard/render-keyboard}))

(defn request-web-midi []
  (let [channel (async/chan)
        midi-access-promise (.requestMIDIAccess js/navigator)
        midi-access (.then midi-access-promise #(async/put! channel %))]
    channel))


(defonce midi-playback-loop
         (let [ch (z/to-chan input-signal)]
           (async/go-loop
             []
             (let [[evt midi] (async/<! ch)]
               (when (= evt :core.action/key-press)
                 (print "play!!!!")
                 (abc/play-abc (abc/midi-to-abc midi)))
               (recur)))))


;; try midi
(async/go
  (let [midi-access (async/<! (request-web-midi))
        device (as-> midi-access _
                     (aget _ "inputs")                      ;; (.-inputs _)
                     (.values _)
                     (.next _)
                     (aget _ "value")                       ;; (.-value _)
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
                  (async/put! input-signal (app/make-action :core.action/key-press pitch)))))))))