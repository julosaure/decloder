(ns decloder.core
  (:require clojure.string)
  (:require clojure.java.io)
  (:import [java.io BufferedReader FileReader])

  (:require decloder.model))


;; GLOBALS


;; UTILS

;; FUNCTIONS 


(defn -main []
  (time (decloder.model/init-engine))
  ;(read-sentences)
  ;(translate-sentences)
  )