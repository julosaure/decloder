(ns decloder.blm
  (:require clojure.java.shell)
  (:require clojure.string)
  (:import [java.util Arrays])
  (:import [edu.berkeley.nlp.lm.io LmReaders])
  (:import [edu.berkeley.nlp.lm NgramLanguageModel])
  )

;; GLOBALS

(def LM_QUERY "java -ea -mx1000m -server -cp ../src edu.berkeley.nlp.lm.io.ComputeLogProbabilityOfTextStream ")

;(def LM_BIN "/Users/julien/workspaces/clojure/berkeleylm-1.1.2/examples/big_test.binary")

(def LM_BIN "/Users/julien/workspaces/xp/europarl/europarl-v7.fr-en.fr.tok.low.se.berk_lmbin")


;; UTILS


;; FUNCTIONS

(defn call-lm [lm list-ngrams]
  (- (.getLogProb lm list-ngrams))
  )



(defn load-lm []
  (println "Loading LM " LM_BIN)
  (LmReaders/readLmBinary LM_BIN)
  )
    
(defn score-ngrams [lm n-grams]
  {:pre [(string? n-grams)
         (> (count n-grams) 0)]
   :post [(pos? %)]}

  (let [list-n-grams (clojure.string/split n-grams #" ")]
    (call-lm lm list-n-grams)
    )
  )
