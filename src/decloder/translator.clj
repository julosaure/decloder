(ns decloder.translator
  (:require clojure.string)
  (:require clojure.java.io)
  (:import [java.io BufferedReader FileReader])
  (:require clojure.data.priority-map)
  )
  


;; GLOBALS

(def MAX_HYPO_PER_STACK 100)

;; UTILS

(defrecord Hypothesis [token, score, pred])

;; FUNCTIONS

(defn score-hypothesis [trg-token lex-prob pred-hypo]
  (+ lex-prob (:score pred-hypo))
  )

(defn new-hypo [stack lex-prob]
  (let [trg-token (second lex-prob)
        lexical-prob (last lex-prob)
        pred (Hypothesis. nil 0 nil)
        score (score-hypothesis trg-token lexical-prob pred)]
    (assoc stack (Hypothesis. trg-token score pred) score)
    )
  )

(defn extend-hypo [model stack hypo src-token]
  (loop [stack_ stack
         lex-probs (filter #(= (first (key %)) src-token) (model :lex-prob))]
    (if (empty? lex-probs)
      stack
      (let [lex-prob (first lex-probs)
            trg-token (second lex-prob)
            lexical-prob (last lex-prob)
            score (score-hypothesis trg-token lexical-prob hypo)]
        (recur (assoc stack_ (Hypothesis. trg-token score hypo) score) (rest lex-probs))
        )
      )
    )
  )

(defn search [model src-sentence]

  (loop [src-sentence_ src-sentence
         pos 0
         stacks {}]

    (let [src-token (first src-sentence_)]
      (println "Main loop, pos " pos ", token " src-token)
      (if (nil? (stacks pos))
        (recur src-sentence_  pos (assoc stacks pos (clojure.data.priority-map/priority-map)])
        
        (if (> pos (count src-sentence_))
          stacks
      
          (if (= pos 0)
            (recur (rest src-sentence_) (+ pos 1) 
                   (reduce new-hypo (stacks 0)
                           (filter #(= (first (key %)) src-token) (model :lex-prob))))
            
            (recur (rest src-sentence_) (+ pos 1) 
                   
                   (loop [stacks_ stacks
                          cur-stack (stacks_ pos)
                          prev-stack (stacks (- pos 1))
                          prev-stack-pos 0
                          top-hypo (nth prev-stack prev-stack-pos)]

                     (if (< (count cur-stack) MAX_HYPO_PER_STACK)
                       (let [cur-stack_ (extend-hypo model cur-stack top-hypo src-token)]
                         (recur (assoc stacks_ pos cur-stack_)
                                cur-stack_
                                prev-stack
                                (+ prev-stack-pos 1)
                                (nth prev-stack (+ prev-stack-pos 1)))
                         )
                       stacks_
                       )
                     )
                   )
            )
          )
        )
      )
    )
  )

  

(defn tokenize-sentence [s]
  (map clojure.string/lower-case
   (clojure.string/split s #" ")
   )
  )

(defn tokens-to-ids [model s]
  (let [voc-src (model :voc-src)]
    (map #(voc-src %) s)
    )
  )

  
(defn translate-sentence [model sentence]
  (println "Translating: " sentence)
  (let [sent-tok (tokenize-sentence sentence)
        sent-tok-id (tokens-to-ids model sent-tok)]
    (println "Tokenized: " sent-tok)
    (println "Ids: " sent-tok-id)
    (search model sent-tok-id)
    )
  )
