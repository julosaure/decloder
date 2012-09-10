(ns decloder.core
  (:require clojure.string)
  (:require clojure.java.io)
  (:import [java.io BufferedReader FileReader]))


;; GLOBALS

(def VOC_SRC "/Users/julien/workspaces/clojure/decloder/data/sentfr/fr-en.trn.src.vcb")

(def VOC_TRG "/Users/julien/workspaces/clojure/decloder/data/sentfr/fr-en.trn.trg.vcb")

(def LEX_PROB "/Users/julien/workspaces/clojure/decloder/data/sentfr/fr-en.t3.final.small")


;; UTILS

;; FUNCTIONS 

(defn read-voc [f]
  (println "Reading vocabulary " f)
  (with-open [rdr (BufferedReader. (FileReader. f))]
    (loop [line (.readLine rdr)
           token_map {}]
      (if line
        (let [tab (clojure.string/split line #" ")
              token_id (first tab)
              token (second tab)]
          (recur (.readLine rdr) (assoc token_map token token_id))
          )
        token_map
        )
      )
    )
  )

  

(defn init-engine []
  (let [voc-src (read-voc VOC_SRC)
        voc-trg (read-voc VOC_TRG)]
        ;(read-lex-prob LEX_PROB)
    voc-src
  ))


(defn -main []
  (init-engine)
  ;(read-sentences)
  ;(translate-sentences)
  )