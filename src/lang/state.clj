(ns lang.state)

(def state (atom {}))

(defn void! []
  (reset! state {}))

(defn get! [path]
  (reduce
   (fn [val step]
     (if (delay? val)
       (get @val step)
       (get val step)))
   @state
   path))
