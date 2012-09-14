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
  ;(println trg-token)
  ;(println lex-prob)
  ;(println (type pred-hypo))
  ;(println (:score pred-hypo))
  (+ lex-prob (:score pred-hypo))
  )

(defn new-hypo [stack lex-prob]
  ;(println lex-prob)
  (let [trg-token (second (key lex-prob))
        lexical-prob (last lex-prob)
        pred (Hypothesis. nil 0 nil)
        score (score-hypothesis trg-token lexical-prob pred)]
    (assoc stack (Hypothesis. trg-token score pred) score)
    )
  )

(defn extend-hypo [model stack top-hypo src-token]
  (loop [stack_ stack
         lex-probs (filter #(= (first (key %)) src-token) (model :lex-prob))
         tata (println "count lex-probs" (count lex-probs))]
    (if (empty? lex-probs)
      stack_
      (let [lex-prob (first lex-probs)
            trg-token (second (key lex-prob))
            lexical-prob (last lex-prob)
            score (score-hypothesis trg-token lexical-prob top-hypo)]
        (recur (assoc stack_ (Hypothesis. trg-token score top-hypo) score) (rest lex-probs) "dd")
        )
      )
    )
  )

(defn search [model src-sentence]

  (loop [src-sentence_ src-sentence
         pos 0
         stacks {}]

    (let [src-token (first src-sentence_)]
      (println "Main loop, pos " pos ", src-token " src-token ", count(stacks) " (count stacks))
      (if (nil? (stacks pos))
        (recur src-sentence_  pos (assoc stacks pos (clojure.data.priority-map/priority-map)))

        (if (nil? src-token)
          (recur (rest src-sentence_) (+ pos 1) stacks)

          (if (= 0 (count (filter #(= (first (key %)) src-token) (model :lex-prob))))
            (recur (rest src-sentence_) (+ pos 1) stacks)
            
            (if (> pos (count src-sentence_))
              stacks
      
              (if (= pos 0)
                (recur (rest src-sentence_) (+ pos 1)
                       (assoc stacks 0
                              (reduce new-hypo (stacks 0)
                                      (filter #(= (first (key %)) src-token) (model :lex-prob)))))
                
                (recur (rest src-sentence_) (+ pos 1) 
                       
                       (loop [stacks_ stacks
                              cur-stack (stacks_ pos)
                              titi (println "count1 " (count cur-stack))
                              prev-stack-pos 1
                              prev-stack (stacks_ (- pos prev-stack-pos))]
                         
                         (if (= 0 (count prev-stack))
                           (let [prev-stack-pos_ (+ 1 prev-stack-pos)
                                 prev-stack_ (stacks_ prev-stack-pos_)
                                 toto (println "recur prev-stack 0")]
                             (recur stacks_ cur-stack "dd" prev-stack-pos_ prev-stack_)
                             )
                           
                           (if (< (count cur-stack) MAX_HYPO_PER_STACK)
                             (let [toto (println "recur cur-stack > 0, count2 " (count cur-stack))
                                   top-hypo (key (first prev-stack))
                                   cur-stack_ (extend-hypo model cur-stack top-hypo src-token)
                                   tata (println "count3 " (count cur-stack_))]
                               (recur (assoc stacks_ pos cur-stack_)
                                      cur-stack_ "dddd"
                                      prev-stack-pos
                                      (rest prev-stack))
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
      )
    )
  )

(defn extract-best-path [graph]
  (let [nb-stacks (count graph)]
    (loop [cpt (- nb-stacks 1)
           cur-stack (graph cpt)
           best-path []]
      (if (< cpt 0)
        (map #(:token %) best-path)
        (recur (- cpt 1) (graph (- cpt 1)) (concat (first cur-stack) best-path))
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

(defn ids-to-tokens [inv-voc-trg ids]
  (map #(inv-voc-trg %) ids)
  )
  
(defn translate-sentence [model sentence]
  (println "Translating: " sentence)
  (let [sent-tok (tokenize-sentence sentence)
        sent-tok-id (tokens-to-ids model sent-tok)]
    (println "Tokenized: " sent-tok)
    (println "Ids: " sent-tok-id)
    (let [graph (search model sent-tok-id)
          best-path (extract-best-path graph)
          inv-voc-trg (reduce #(assoc %1 (val %2) (key %2)) {} (model :voc-trg))
          ];tt (println (take 10 inv-voc-trg))]
      (println best-path)
      (println (ids-to-tokens inv-voc-trg best-path))
      )
    )
  )
