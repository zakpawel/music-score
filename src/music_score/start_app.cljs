(ns music-score.start-app
  (:require
    [rum.core :as rum]
    [jamesmacaulay.zelkova.signal :as z]
    [cljs.core.async.impl.protocols :as async-impl]
    [cljs.core.async :as async])
  (:require-macros
    [cljs.core.async.macros :refer [go go-loop]]))

(def make-action vector)

(defn proxy [f o]
  (reify
    async/Mult
    (tap* [m ch close?]
      (async/tap* m ch close?))
    (untap* [m ch]
      (async/untap* m ch))
    (untap-all* [m]
      (async/untap-all* m))
    async-impl/WritePort
    (put! [_ val fn1-handler]
      (async-impl/put! o (f val) fn1-handler))))

(defn map-signal [f signal]
  (let [in (->> signal
                #_(z/map f)
                (z/to-chan))
        out (z/write-port [::nothing])]
    (z/pipe-to-atom signal)
    (z/pipe-to-atom out)
    (go
      (loop []
        (let [v (async/<! in)]
          (print "start_app map-signal loop" v)
          (async/put! out (f v))
          (recur))))
    out))

(declare start-adv)

(defn start [config]
  (start-adv (assoc-in config [:inputs] []))
  #_(let [signal
        (z/write-port [::nothing])

        model-signal
        (z/foldp update init signal)

        html-signal
        (z/map (fn [model]
                 (rum/mount (view signal model)
                            (. js/document (getElementById "app")))) model-signal)]
    (z/pipe-to-atom html-signal)
    signal))



;; Config
;; ------
;; init   : (model, Effects action)
;; update : action -> model -> (model, Effects action)
;; view   : Address action -> model -> Html
;; inputs : List (Signal action)


(defn start-adv
  ([config]
    (start-adv config (. js/document getElementById "app")))
  ([{:keys [init update view inputs]} node]
   (let [signal
         (z/write-port [::nothing])

         all-signals
         (z/mergeseq (conj inputs signal))

         model-signal
         (z/foldp update init all-signals)

         html-signal
         (z/map (fn [model]
                  (rum/mount (view signal model)
                             node)) model-signal)]
     (z/pipe-to-atom html-signal)
     signal)))