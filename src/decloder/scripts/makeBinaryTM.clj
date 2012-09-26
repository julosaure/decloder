(ns decloder.scripts.makeBinaryTM
  (:require decloder.model)
  (:import [java.io ObjectOutputStream OutputStream BufferedOutputStream FileOutputStream])
  )

(defn -main [inputTM outputBinaryTM]
  ; Reads a giza translation model and serializr it
  (let [model (decloder.model/read-lex-prob inputTM)
        out (ObjectOutputStream. (BufferedOutputStream. (FileOutputStream. outputBinaryTM)))]
    (doto out
      (.writeObject model)
      (.close)
      )
    )
  )
   