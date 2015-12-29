(ns music-score.utils
  (:require [cljs.analyzer.api :as ana-api]))

(defmacro check-flag [flag-sym pred & forms]
  `(when (~pred (aget ~(->> *ns*
                            (ns-name))
                      (name ~flag-sym)))
     ~@forms))

(defmacro print [& msgs]
  `(check-flag 'debug (fn [flag#] (> flag# 0)) (print ~@msgs)))

(defmacro println [& msgs]
  `(print ~@msgs))

(defmacro debug [form]
  `(let [v# ~form]
     (print '~form "=" v#)
     v#))
