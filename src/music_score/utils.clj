(ns music-score.utils)


(defmacro debug [form]
  `(let [v# ~form]
     (print '~form "=" v#)
     v#))
