(ns lang.reference)

(defn is?
  [form]
  (and (associative? form) (:ast/reference form)))

(defn canonicalize
  [form]
  {:pre [(is? form)]}
  (select-keys form [:ast/reference :name]))
