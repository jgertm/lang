(ns lang.ast)

(defn module?
  [form]
  (and
    (map? form)
    (= :module (get form :ast/definition))))

(defn definition?
  [form]
  (and
    (map? form)
    (contains? form :ast/definition)))
