(ns tutorial.system
  (:require [tutorial.core :as c]
            [clojure.core.async :refer [>! <!! go chan]]))

(defn system
    "Returns a new instance of the whole application."
      [])

(defn start
      "Performs side effects to initialize the system, acquire resources,
        and start it running. Returns an updated instance of the system."
        [system]
          system)

(defn stop
    "Performs side effects to shut down the system and release its
      resources. Returns an updated instance of the system."
      [system]
        system)
