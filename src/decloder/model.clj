(ns decloder.model
  (:require clojure.string)
  (:require clojure.java.io)
  (:import [java.io BufferedReader FileReader InputStream ObjectInputStream FileInputStream BufferedInputStream InputStreamReader ])
  (:import [java.nio.channels Channels])
  (:import [java.util.zip GZIPInputStream])
  (:import [java.lang Math])
  (:require decloder.blm) 
  (:require clojure.data.priority-map)
  )


;; GLOBALS

(def VOC_SRC "./data/sentfr/fr-en.trn.src.vcb")

(def VOC_TRG "./data/sentfr/fr-en.trn.trg.vcb")

(def LEX_PROB "./data/sentfr/fr-en.t3.final.bin") ;.pc10.bin")

(def PC_FILTER_LEX_PROBS 0.1)

(def MIN_KEEP_LEX_PROBS 10)

;; UTILS

;; FUNCTIONS 

(defn read-voc [f]
  {:post [(vector? %)
          (= true (reduce #(and %1 %2) (map #(map? %) %)))]}
  
  (println "Reading vocabulary " f)
  (with-open [rdr (BufferedReader. (InputStreamReader. (FileInputStream. f) "UTF-8"))]
    (loop [line (.readLine rdr)
           token_id_map {}
           id_token_map {}]
      (if line
        (let [tab (clojure.string/split line #" ")
              token_id (first tab)
              token (second tab)]
          (recur (.readLine rdr) (assoc token_id_map token token_id) (assoc id_token_map token_id token))
          )
        (do
          (println (count token_id_map) " tokens read.")
          [token_id_map id_token_map]
          )
        )
      )
    )
  )

(defn read-lex-prob-bin [f]
  (let [fis (FileInputStream. f)
        channel (.getChannel fis)
        cis (Channels/newInputStream channel)
        bis (BufferedInputStream. cis)
        bis (if (= ".gz" (subs f (- (count f) 3))) (GZIPInputStream. bis) bis)
        ois (ObjectInputStream. bis)
        unserializedModel (.readObject ois)]
    (.close ois)
    (println "Unserialized translation model with " (count unserializedModel) " lexical probabilities.")
    unserializedModel
    )
  )
  

(defn read-lex-prob_ [f]
  {:post [(map? %)]}
  
  (with-open [rdr (BufferedReader. (FileReader. f))]
    (loop [i 0
           line (.readLine rdr)
           lex_prob_map (transient {})]
      (if line
        (let [tab (clojure.string/split line #" ")
              token_src (first tab)
              token_trg (second tab)
              lex_prob (last tab)
              minus_log_lex_prob (- (Math/log (Double. lex_prob)))]
          (if (nil? (lex_prob_map token_src))
            (recur (+ i 1) (.readLine rdr) (assoc! lex_prob_map token_src (clojure.data.priority-map/priority-map token_trg minus_log_lex_prob)))
            (recur (+ i 1) (.readLine rdr) (assoc! lex_prob_map token_src (assoc (lex_prob_map token_src) token_trg minus_log_lex_prob)))
            )
          )
        (do
          (println i " lexical probabilities read for " (count lex_prob_map) " src tokens.")
          (persistent! lex_prob_map)
          )
        )
      )
    )
  )

(defn read-lex-prob [f]
  {:post [(map? %)]}

  (println "Reading lexical probabilities " f)
  (if (or (= ".gz" (subs f (- (count f) 3)))
          (= ".bin" (subs f (- (count f) 4))))
    (read-lex-prob-bin f)
    (read-lex-prob_ f)
    )
  )

(defn filter-lex-probs [lex-probs percent-filter]
  {:post [(= (count lex-probs) (count %))]}
  
  (loop [filtered-lex-probs (transient {})
         seq_ (seq lex-probs)
         nb-lex-probs 0]
    (if (empty? seq_)
      (let [_ (println "Filtered model has " nb-lex-probs " lex probs for " (count filtered-lex-probs) " src tokens.")]
        (persistent! filtered-lex-probs))
      (let [[src-tok list-trg-probs] (first seq_) 
            max_ (max MIN_KEEP_LEX_PROBS (int (* percent-filter (count list-trg-probs))))
            ];_ (println src-tok ":" (count list-trg-probs) ":" max_)]
        (recur (assoc! filtered-lex-probs src-tok (take max_ list-trg-probs)) (rest seq_) (+ nb-lex-probs max_))))))

  
(defn init-engine []
  {:post [(map? %)]}
  
  (let [[voc-src-id voc-id-src] (read-voc VOC_SRC)
        [voc-trg-id voc-id-trg] (read-voc VOC_TRG)
        lex-probs (read-lex-prob LEX_PROB)
        ;lex-probs (filter-lex-probs lex-probs PC_FILTER_LEX_PROBS)
        lm (decloder.blm/load-lm)]
    ;(println (sort (filter #(.startsWith (key %) "ann") voc-src)))
    {:voc-src-id voc-src-id, :voc-id-src voc-id-src, :voc-trg-id voc-trg-id, :voc-id-trg voc-id-trg, :lex-prob lex-probs, :lm lm}
  ))

