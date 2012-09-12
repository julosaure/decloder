(ns decloder.translator
  (:require clojure.string)
  (:require clojure.java.io)
  (:import [java.io BufferedReader FileReader])
  )
  


;; GLOBALS

(def MAX_HYPO_PER_STACK 100)

;; UTILS

;(def hypothesis [token, pos, score, pred])

;; FUNCTIONS

(defn score-hypothesis [model hypothesis]

  )

(defn add-hypo [stack hypo]
  (assoc stack hypo)
  )

(defn search [model sentence]

  (loop [token (first sentence)
         pos 0
         stacks {}]

    (if (nil? (stacks pos))
      (recur sentence pos (assoc stacks pos {}))
      (do
        (while (< (count (stacks pos)) MAX_HYPO_PER_STACK)
          (if (= pos 0)
            (reduce add-hypo (stacks 0)
                    (filter #(= (first (key %)) token) (model :lex-prob))
                    )
            (

             )
            )
          )
          (recur (rest sentence) (+ pos 1) stacks)
        )
      )
    )
  )


(defn translate-sentence [model sentence]
  
  )
