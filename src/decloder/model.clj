(ns decloder.model
  (:require clojure.string)
  (:require clojure.java.io)
  (:import [java.io BufferedReader FileReader InputStream ObjectInputStream FileInputStream BufferedInputStream InputStreamReader ])
  (:import [java.nio.channels Channels])
  (:import [java.util.zip GZIPInputStream])
  (:import [java.lang Math])
  (:require decloder.blm) 
  )


;; GLOBALS

(def VOC_SRC "/Users/julien/workspaces/clojure/decloder/data/sentfr/fr-en.trn.src.vcb")

(def VOC_TRG "/Users/julien/workspaces/clojure/decloder/data/sentfr/fr-en.trn.trg.vcb")

(def LEX_PROB "/Users/julien/workspaces/clojure/decloder/data/sentfr/fr-en.t3.final.bin")

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
   ;  final InputStream fis = getBufferedInputStream(path);
  ; return path.getName().endsWith(".gz") ? new ObjectInputStream(new GZIPInputStream(fis)) : new ObjectInputStream(fis);      }
  ;final ObjectInputStream in = openObjIn(path);
  ;final Object obj = in.readObject();
  ;                in.close();
  

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
            (recur (+ i 1) (.readLine rdr) (assoc! lex_prob_map token_src {token_trg minus_log_lex_prob}))
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

(defn startswith [str pat]
  (loop [str_ str
         pat_ pat]
    (if pat_
      (if (= (first pat_) (first str_))
        (recur (rest str_) (rest pat_))
        false
        )
      true
      )
    )
  )

(defn init-engine []
  {:post [(map? %)]}
  
  (let [[voc-src-id voc-id-src] (read-voc VOC_SRC)
        [voc-trg-id voc-id-trg] (read-voc VOC_TRG)
        lex-prob (read-lex-prob LEX_PROB)
        lm (decloder.blm/load-lm)]
    ;(println (sort (filter #(.startsWith (key %) "ann") voc-src)))
    {:voc-src-id voc-src-id, :voc-id-src voc-id-src, :voc-trg-id voc-trg-id, :voc-id-trg voc-id-trg, :lex-prob lex-prob, :lm lm}
  ))

