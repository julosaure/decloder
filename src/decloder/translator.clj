(ns decloder.translator
  (:require clojure.string)
  (:require clojure.java.io)
  (:import [java.io BufferedReader FileReader])
  (:require clojure.data.priority-map)
  (:require decloder.blm)
  )
  


;; GLOBALS

(def MAX_HYPO_PER_STACK 100)

;; UTILS

(defrecord Hypothesis [token, score, pred])

;; FUNCTIONS

(defn score-hypothesis [model trg-token lex-prob pred-hypo]
  {:pre [(string? trg-token)
         (= java.lang.Double (type lex-prob))
         (>= lex-prob 0)
         (or (nil? pred-hypo) (= decloder.translator.Hypothesis (type pred-hypo)))]
   :post [(>= % 0) (>= % lex-prob)]}

  (if (nil? pred-hypo)
    lex-prob
    (let [bi-gram (str (:token pred-hypo) " " trg-token)]
      (if (nil? (:pred pred-hypo))
        (let [lm-score (decloder.blm/score-ngrams (model :lm) bi-gram)]
          ;(println "bi-gram to score: " bi-gram " -> " lm-score)
          (+ lex-prob (* 0.5 (:score pred-hypo)) (* 0.1 lm-score))
          )
        (let [tri-gram (str (:token (:pred pred-hypo)) " " bi-gram)]
          (if (nil? (:pred (:pred pred-hypo)))
            (let [lm-score (decloder.blm/score-ngrams (model :lm) tri-gram)]
              ;(println "tri-gram to score: " tri-gram " -> " lm-score)
              (+ lex-prob (* 0.3 (:score pred-hypo)) (* 0.15 lm-score))
              )
            (let [quad-gram (str (:token (:pred (:pred pred-hypo))) " " tri-gram)
                  lm-score (decloder.blm/score-ngrams (model :lm) quad-gram)]
              ;(println "4-gram to score: " quad-gram " -> " lm-score)
              (+ lex-prob (* 0.3 (:score pred-hypo)) (* 0.15 lm-score)))))))))

(defn new-hypo [model stack lex-prob]
  {:pre [(map? stack)
         (= clojure.lang.MapEntry (type lex-prob))]
   :post [(map? %)
          (>= (count %) (count stack))]}

  (let [trg-token-id (key lex-prob)
        trg-token ((model :voc-id-trg) trg-token-id)
        lexical-prob (val lex-prob)
        pred (Hypothesis. nil 0 nil)
        score (score-hypothesis model trg-token lexical-prob pred)]
    (assoc stack (Hypothesis. trg-token score pred) score)
    )
  )

(defn extend-hypo [model stack prev-hypo src-token]
  {:pre [(map? model)
         (map? stack)
         (or (nil? prev-hypo) (= decloder.translator.Hypothesis (type prev-hypo)))
         (= java.lang.String (type src-token))]
   :post [(map? %)
          (>= (count %) (count stack))]}

  (let [src-token-id ((model :voc-src-id) src-token)]
    (loop [stack_ stack
           lex-probs ((model :lex-prob) src-token-id)]
      (if (empty? lex-probs)
        stack_
        (let [lex-prob (first lex-probs)
              trg-token-id (key lex-prob)
              lexical-prob (val lex-prob)
              trg-token ((model :voc-id-trg) trg-token-id)
              score (score-hypothesis model trg-token lexical-prob prev-hypo)
              ; way faster than (last stack_)
              worst-hypo (first (rseq stack_))]
          (if (or (nil? worst-hypo) (< score (:score (key worst-hypo))))
            ; we had the new hypo tho the queue only if its score is
            ; better than the worst in the queue
            (recur (assoc stack_ (Hypothesis. trg-token score prev-hypo) score) (rest lex-probs))
            (recur stack_ (rest lex-probs))))))))


(defn count-stacks [stacks]
  (loop [stacks_ stacks
         msg ""]
    (if (empty? stacks_)
      msg
      (let [first-key (first (sort (keys stacks_)))
            stack (stacks first-key)
            msg (str msg " " first-key ":" (count stack))]
        (recur (dissoc stacks_ first-key) msg)))))

(defn shave-stack [stack]
  {:pre [(= clojure.data.priority_map.PersistentPriorityMap (type stack))]
   :post [(= clojure.data.priority_map.PersistentPriorityMap (type %))]}

  (if (> (count stack) MAX_HYPO_PER_STACK)
    (let [s1 (take MAX_HYPO_PER_STACK stack)]
      (reduce #(apply assoc %1 %2) (clojure.data.priority-map/priority-map) s1)) 
    stack))


(defn search-first-not-empty-prev-stack [stacks pos]
  {:post [(map? %)]}
  
  (loop [prev-stack-not-empty-pos 1]
    (let [prev-stack (stacks (- pos prev-stack-not-empty-pos))]
      (if (= 0 (count prev-stack))
        (recur (+ 1 prev-stack-not-empty-pos))
        prev-stack))))


(defn extend-stack [stack prev-stack model src-token]
  {:post [(>= (count %) (count stack))]}

  (loop [stack_ stack
         prev-stack_ prev-stack]
    (if (empty? prev-stack_)
      stack_
      (let [top-hypo (key (first prev-stack_))
            stack_ (extend-hypo model stack_ top-hypo src-token)
            stack_ (shave-stack stack_)]
        (recur stack_ (rest prev-stack_)))))) 


(defn search [model src-sentence]

  (loop [src-sentence_ src-sentence
         pos 0
         stacks {}]

    (let [src-token (first src-sentence_)
          src-token-id ((model :voc-src-id) src-token)]
      (println "Main loop, pos " pos ", src-token " src-token ", count(stacks) " (count-stacks stacks))
      (if (nil? (stacks pos))
        (recur src-sentence_  pos (assoc stacks pos (clojure.data.priority-map/priority-map)))

        (if (= 0 (count src-sentence_))
          stacks
              
          (if (nil? src-token)
            (recur (rest src-sentence_) (+ pos 1) stacks)

          
            (if (= 0 (count ((model :lex-prob) src-token-id)))
              (recur (rest src-sentence_) (+ pos 1) stacks) 
            
            
              (if (= pos 0)
                (let [hypos ((model :lex-prob) src-token-id)
                      ;hypos (filter #(= (first (key %)) src-token) (model :lex-prob))
                      ;_ (println "(count fist hypos) " (count hypos))
                      ;_ (println hypos)
                      stack_ (reduce (partial new-hypo model) (stacks 0) hypos)
                      ];tata (println "(count stack_) " (count stack_))]
                  (recur (rest src-sentence_) (+ pos 1) (assoc stacks 0 stack_)))

                (let [prev-stack (search-first-not-empty-prev-stack stacks pos)
                      stack (stacks pos)
                      stack (extend-stack stack prev-stack model src-token)
                      stack (shave-stack stack)]
                  (recur (rest src-sentence_) (+ pos 1) (assoc stacks pos stack)))))))))))
                  

(defn extract-best-path [graph]
  (let [nb-stacks (count graph)]
    (loop [cpt (- nb-stacks 1)]
      (let [cur-stack (graph cpt)]
        (if (= 0 (count cur-stack))
          (recur (- cpt 1))
          (loop [hypo (key (first cur-stack))
                 best-path []]
            (if (nil? (:pred hypo))
              best-path
              (do
                ;(println "c" best-path)
                ;(println "d" hypo)
                (recur (:pred hypo) (concat [(:token hypo)] best-path))
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
  (let [voc-src (model :voc-src-id)]
    (map #(voc-src %) s)
    )
  )

(defn ids-to-tokens [inv-voc-trg ids]
  (map #(inv-voc-trg %) ids)
  )

(defn filter-src-lex-probs [model sent-tok-ids]
  (println "Filtering lexical probabilities")
  (loop [tokens sent-tok-ids
         new-lex-probs {}]
    (if (empty? tokens)
      (do
        (println (count new-lex-probs) " lex probs remaining.")
        (assoc model :lex-prob new-lex-probs))
      (let [fil (filter #(= (first (key %)) (first tokens)) (model :lex-prob))]
        (println "Filtered " (count fil) " lex probs.")
        (recur (rest tokens) (merge new-lex-probs fil)) 
        )
      )
    )
  )

(defn translate-sentence [model sentence]
  (println "Translating: " sentence)
  (let [sent-tok (tokenize-sentence sentence)
        ];sent-tok-id (tokens-to-ids model sent-tok)]
    (println "Tokenized: " sent-tok)
    ;(println "Ids: " sent-tok-id)
    (let [;model (filter-src-lex-probs model sent-tok-id)
          graph (search model sent-tok)
          best-path (extract-best-path graph)
          ;inv-voc-trg (reduce #(assoc %1 (val %2) (key %2)) {} (model :voc-trg))
          ];tt (println (take 10 inv-voc-trg))]
      (println best-path)
      ;(println (ids-to-tokens inv-voc-trg best-path))
      )
    )
  )
