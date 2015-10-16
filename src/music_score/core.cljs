(ns music-score.core
  (:require [cljs.core.async :as async]
            [clojure.data :as data]
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
   :config/key         {}
   :config/range       {}
   :game/config        {:db/valueType :db.type/ref :db/cardinality :db.cardinality/one}
   :game/stage         {}
   :game/exercise      {:db/valueType :db.type/ref :db/cardinality :db.cardinality/many}
   :exercise/from      {}
   :exercise/to        {}
   :exercise/clef      {}
   :exercise/timestamp {}
   :exercise/guess     {:db/valueType :db.type/ref :db/cardinality :db.cardinality/many}
   :guess/question     {}
   :guess/answer       {}
   :guess/timestamp    {}})


(let [conn (d/create-conn schema)
     mk-id (let [id (atom 0)]
             (fn []
               (swap! id dec)
               @id))
     mk-guess
     (fn mk-guess
       ([q a]
        (as-> {:guess/timestamp (Date.now)
               :guess/question  q
               :guess/answer    a
               :db/id           (mk-id)} _
              (if a
                _
                (dissoc _ :guess/answer))))
       ([q] (mk-guess q nil)))

     random-int (fn [from to]
                  (->> to
                       (inc)
                       (rand-int)
                       (+ from)))


     mk-exercise
     (fn [from to clef]
       {:db/id              (mk-id)
        :exercise/from      from
        :exercise/to        to
        :exercise/clef      clef
        :exercise/timestamp (Date.now)
        :exercise/guess     (->> (range 0 5)
                                 (map #(mk-guess (random-int from to) nil #_(random-int from to)))
                                 (into []))})
     records (map #(mk-exercise 1 3 :bass) (range 1))]
 (print records)
 (print (->> records
             (map :exercise/guess)
             #_((fn [a] (print a) a))
             (map (fn [s]
                    (->> s
                         (map #(vector (% :db/id)
                                       (% :guess/question)
                                       (% :guess/answer)) s)
                         (filter (fn [[id q a]] (= q a))))))))

 (d/transact! conn records)
 #_(d/transact! conn [{:exercise/guess [(-> (mk-guess (random-int 10 15) (random-int 10 15))
                                          (assoc :db/id 2)
                                          ((fn [a] (print a) a))
                                          )]}])
 (let [result (d/q '[:find ?q ?a ?g
                     :in $
                     :where
                     [?e :exercise/guess ?g]
                     [?g :guess/question ?q]
                     [(get-else $ ?g :guess/answer nil) ?a]
                     ] @conn)]
   (->> result (print) #_(clj->js) (js/console.log)))
 (->> @conn (print))
 )

      (let [schema {:aka {:db/cardinality :db.cardinality/many}}
            conn (d/create-conn schema)]
        (d/transact! conn [{:db/id -1
                            :name  "asdasdasd"
                            :age   45
                            :aka   ["Maks Otto von Stirlitz", "Jack Ryan"]}
                           {:db/id -2
                            :name  "dupa"
                            :age   43
                            :aka   ["Maks Otto von Stirlitz", "Jack Ryan"]}])
        (d/transact! conn [{:db/id -1
                            :name  "Maksim"
                            :age   45
                            :aka   ["Maks Otto von Stirlitz", "Jack Ryan"]}])
        (let [result (d/q '[:find ?n ?a
                            :where [?e :aka "Maks Otto von Stirlitz"]
                            [?e :name ?n]
                            [?e :age ?a]]
                          @conn)]
          (->> result (clj->js) (.log js/console))))

;; => #{ ["Maksim" 45] }


;; Destructuring, function call, predicate call, query over collection

(comment
 (d/q '[:find ?k ?x
        :in [[?k [?min ?max]] ...] ?range
        :where [(?range ?min ?max) [?x ...]]
        [(even? ?x)]]
      {:a [1 7], :b [2 4]}
      range)

 ;; => #{ [:a 2] [:a 4] [:a 6] [:b 2] }


 ;; Recursive rule

 (d/q '[:find ?u1 ?u2
        :in $ %
        :where (follows ?u1 ?u2)]
      [[1 :follows 2]
       [2 :follows 3]
       [3 :follows 4]]
      '[[(follows ?e1 ?e2)
         [?e1 :follows ?e2]]
        [(follows ?e1 ?e2)
         [?e1 :follows ?t]
         (follows ?t ?e2)]])

 ;; => #{ [1 2] [1 3] [1 4]
 ;;       [2 3] [2 4]
 ;;       [3 4] }


 ;; Aggregates

 (d/q '[:find ?color (max ?amount ?x) (min ?amount ?x)
        :in [[?color ?x]] ?amount]
      [[:red 10] [:red 20] [:red 30] [:red 40] [:red 50]
       [:blue 7] [:blue 8]]
      3))


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
   :finished (start-game model)
   :guessing
   (let [model' (update-in model [:answer] conj midi)
         answer (model' :answer)
         question (model' :question)]
     (if (= answer question)
       (assoc-in model' [:stage] :finished)
       (let [ans-count (count answer)
             q-count (count question)]
         (if (< ans-count q-count)
           model'
           (assoc-in model' [:stage] :finished))
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


(defn handle-drag [model dragging?]
 (assoc model :dragging dragging?))


(defn update-model [[event-type & args] model]
  (print "update-model" event-type args)
  (condp = event-type
    :start-game (start-game model)
    :key-press (key-press model (first args))
    :config (config model args)
    :dragging (handle-drag model (first args))
    model
    ))

;; here foldp
(defonce state-signal
         (z/foldp update-model initial-model input-signal))

;; render function
(defn render-app [state]
  (rum/mount (ui/root-component state input-signal) (. js/document (getElementById "app"))))

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