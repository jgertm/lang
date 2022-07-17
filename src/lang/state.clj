(ns lang.state)

(def state (atom {}))

(defn void! []
  (reset! state {}))

(defn get! [path]
  (->> path
       (reduce
        (fn [val step]
          (get (force val) step))
        @state)
       force))
