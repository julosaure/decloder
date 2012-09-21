(ns decloder.kenlm
  (:require clojure.java.shell)
  (:require clojure.string)
  )

;; GLOBALS

(def LM_QUERY "/Users/julien/workspaces/xp/kenlm/query")

(def LM_BIN "/Users/julien/workspaces/clojure/decloder/data/lm/lm_giga_64k_vp_3gram/lm_giga_64k_vp_3gram.kenbinary")

;; UTILS


;; FUNCTIONS

(defn call-lm [n-grams]
  (let [res (clojure.java.shell/sh LM_QUERY LM_BIN :in n-grams)]
    (if (> 0 (count (:err res)))
      (println "Error while querying LM with " n-grams ": " (:err res))
      (:out res)
      )
    )
  )

(def TOTAL_PAT #"Total: ([-0-9.]+) ")

(defn parse-lm-output-line [line]
  (let [matcher (re-matcher TOTAL_PAT line)
        score (re-find matcher)]
    (println line)
    ;(println (map #(str "X" % "X") score))
    ;(println score)
    (Double. (second score))
    )
  )


(defn parse-lm-output [out list-n-grams]
  (let [first-word (first list-n-grams)
        first-word-escaped (clojure.string/escape first-word {\( "\\(", \) "\\)", \* "\\*", \? "\\?"})
        ;tt (println first-word-escaped)
        pat (re-pattern first-word-escaped)]
    (loop [lines (clojure.string/split-lines out)]
      (if (re-find pat (first lines))
        (parse-lm-output-line (first lines))
        (recur (rest lines))
        )
      )
    )        
  )

    
(defn score-ngrams [n-grams]
  {:pre [(string? n-grams)
         (> (count n-grams) 0)]
   :post [(pos? %)]}
  (let [list-n-grams (clojure.string/split n-grams #" ")
        out (call-lm n-grams)]
    (- (parse-lm-output out list-n-grams))
    )
  )
