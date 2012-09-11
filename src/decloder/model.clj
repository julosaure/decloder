(ns decloder.model
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
        (do
          (println (count token_map) " tokens read.")
          token_map
          )
        )
      )
    )
  )

(defn read-lex-prob [f]
  (println "Reading lexical probabilities " f)
  (with-open [rdr (BufferedReader. (FileReader. f))]
    (loop [line (.readLine rdr)
           lex_prob_map (transient {})]
      (if line
        (let [tab (clojure.string/split line #" ")
              token_src (first tab)
              token_trg (second tab)
              lex_prob (last tab)]
          (recur (.readLine rdr) (assoc! lex_prob_map [token_src token_trg] (Double. lex_prob)))
          )
        (do
          (println (count lex_prob_map) " lexical probabilities read.")
          (persistent! lex_prob_map)
          )
        )
      )
    )
  )

(defn init-engine []
  (let [voc-src (read-voc VOC_SRC)
        voc-trg (read-voc VOC_TRG)
        lex-prob (read-lex-prob LEX_PROB)]
       ;lex-prob
    {:voc-src voc-src, :voc-trg voc-trg, :lex-prob lex-prob}
  ))

