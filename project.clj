(defproject decloder "1.0.0-SNAPSHOT"
  :description "FIXME: write description"

  :repositories {"local" ~(str (.toURI (java.io.File. "mvn_repo")))}

  ; maven groupID/artifactID version
  :dependencies [[org.clojure/clojure "1.3.0"]
                 [org.clojure/data.priority-map "0.0.2"]
                 [berkeleylm/berkeleylm "1.1.2"]
                 ]
  
  :jvm-opts ["-Xmx10242m"]
  :main decloder.core)