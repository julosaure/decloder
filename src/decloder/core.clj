(ns decloder.core
  (:require clojure.string)
  (:require clojure.java.io)
  (:import [java.io BufferedReader FileReader])

  (:require decloder.model)
  (:require decloder.translator))


;; GLOBALS


;; UTILS

;; FUNCTIONS 

(defn read-sentences []
  ;"Il y a quelques années , l' astronome français Alfred Vidal-Majar donnait à l' un de ses
                                        ;ouvrages le joli titre Il
                                        ;pleut des planètes ."
  "Protests expanded to nearly 20 countries , as demonstrators breached the American Embassy in Tunisia and protesters in Sudan broadened their targets to include Germany and Britain ."
  )

(defn -main []
  (let [model (decloder.model/init-engine)
        sentence (read-sentences)]
    (decloder.translator/translate-sentence model sentence)
    )
  )

