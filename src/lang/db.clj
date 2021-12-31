(ns lang.db
  (:require [clojure.walk :as walk]))

(defrecord Ref [eid tempid lookup])

(defmethod print-method Ref
  [{:keys [eid tempid lookup] :as e} ^java.io.Writer w]
  (cond
    eid (.write w (format "#ref[%d]" eid))
    tempid (.write w (format "#temp[%s]" (pr-str tempid)))
    lookup (.write w (format "#lookup%s" (pr-str lookup)))))

(defn ->ref
  [x]
  (cond
    (pos-int? x)
    (map->Ref {:eid x})

    (vector? x)
    (map->Ref {:lookup x})

    :else
    (map->Ref {:tempid x})))

(defn ref? [x]
  (instance? Ref x))

(defrecord Datom
    [^clojure.lang.Keyword m
     ^int e
     ^clojure.lang.Keyword a
     v
     ^Ref tx])

(comment
  (Datom. :db/add 1 :foo "bar" 2)

  )

(defrecord DB [datoms])

(defn init []
  (->DB []))

(defn create []
  (atom (init)))

(defn- unnest-map
  [fact]
  (let [fact (merge {:db/id (gensym)} fact)
        maps (atom [])]
    (walk/postwalk
     (fn [sub]
       (if (and (map? sub) (contains? sub :db/id))
         (do (swap! maps conj sub)
             (->ref (:db/id sub)))
         sub))
     fact)
    @maps))

(comment
  (unnest-map
   {:db/id -1
    :foo :bar
    :baz [{:db/id -2 :baz :quux}
          {:db/id -3 :quux :baz}]})

  )

(defn- sequence-map
  [fact]
  (->> (dissoc fact :db/id)
       (map (fn [[k v]] [:db/add (:db/id fact) k v]))))

(defn- expand-fact
  [fact]
  (letfn [(list-form? [fact]
            (and (vector? fact)
                 (contains? #{:db/add :db/retract} (first fact))
                 (= 4 (count fact))))
          (map-form? [fact]
            (map? fact))]
    (cond
      (list-form? fact)
      [(zipmap [:m :e :a :v] fact)]

      (map-form? fact)
      (->> fact
           (unnest-map)
           (mapcat sequence-map)
           (map (partial zipmap [:m :e :a :v])))

      :else (throw (ex-info "unrecognized fact" {:fact fact})))))

(comment
  (expand-fact [:db/add 1 :foo :bar])

  (expand-fact {:foo :bar
                  :bar "baz"})

  (expand-fact {:foo :bar
                  :baz {:db/id -1 :name "ba"}})

  )

(defn- next-txid
  [db]
  (->> db
       :datoms
       (map :e)
       (reduce max 0)
       inc))

(defn- reify-eids
  [txid facts]
  (let [tempids
        (zipmap (->> facts
                     (keep #(when-let [tempid (:tempid (->ref (:e %)))] tempid))
                     distinct)
                (map (partial + txid 1) (range)))
        temprefs (->> tempids
                     (map (fn [[k v]] [(->ref k) (->ref v)]))
                     (into {}))]
    {:tempids tempids
     :facts
     (map
      (fn [fact]
        (-> fact
            (update :e #(get tempids % %))
            (update :v (partial walk/postwalk-replace temprefs))))
      facts)}))

(defn with
  ([db facts] (with db facts nil))
  ([db facts tx-meta]
   (let [txid (next-txid db)
         {:keys [facts tempids]}
         (->> facts
              (mapcat expand-fact)
              (reify-eids txid))
         tx-facts (map (fn [[k v]] {:m :db/add :e txid :a k :v v}) tx-meta)
         datoms (->> (concat tx-facts facts)
                     (map #(->Datom (:m %) (:e %) (:a %) (:v %) (->ref txid))))]
     {:db-before db
      :db-after (update db :datoms into datoms)
      :tx-data datoms
      :tempids tempids
      :tx-meta tx-meta})))

(comment
  (with (init) [[:db/add -1 :foo "bar"]
                {:db/id 3 :baz :quux :name "Steve"}])

  )

(defn transact!
  ([conn facts] (transact! conn facts nil))
  ([conn facts tx-meta]
   (let [report (with @conn facts tx-meta)]
     (reset! conn (:db-after report))
     report)))

(defn datoms [db]
  (map (juxt :m :e :a :v :tx) (:datoms db)))

(defn index [db order]
  (reduce
   (fn [acc datom]
     (let [{:keys [m e a v tx]} datom
           path (case order
                  :eavt [e a v tx]
                  :avet [a v e tx])]
       (case m
         :db/add (assoc-in acc path datom))))
   nil
   (:datoms db)))

(defn get-latest
  [index k1 k2]
  (some->> (get-in index [k1 k2])
           (apply max-key #(->> % val keys (apply max)))
           key))

(defn dereference
  [db {:keys [eid tempid lookup] :as ref}]
  {:pre [(ref? ref)]}
  (cond
    eid
    eid

    tempid
    (throw (ex-info "cannot dereference tempid" {:tempid tempid}))

    lookup
    (let [[a v] lookup]
      (get-latest (index db :avet) a v))))

(declare ->Entity)

(deftype Entity
    [^DB db ^Ref ref]
  Object
  (toString [e] (pr-str (into {} (seq e))))

  clojure.lang.Seqable
  (seq [_]
    (let [eid (dereference db ref)
          as (keys (get (index db :eavt) eid))]
      (some->> as
               (map (fn [a] [a (get-latest (index db :eavt) eid a)]))
               (cons [:db/id eid]))))

  clojure.lang.ILookup
  (valAt [this a]
    (let [v (get (into {} (.seq this)) a)]
      (cond
        (ref? v)
        (->Entity db v)

        (and (vector? v) (every? ref? v))
        (mapv (partial ->Entity db) v))))
  (valAt [this a not-found] (or (.valAt this a) not-found))

  clojure.lang.IFn
  (invoke [this a] (.valAt this a))
  (invoke [this a not-found] (.valAt this a not-found)))

(defmethod print-method Entity
  [e ^java.io.Writer w]
  (.write w (str e)))

(defn entity? [x]
  (instance? Entity x))

(defn ->entity [db x]
  (if (ref? x)
    (->Entity db x)
    (->Entity db (->ref x))))

(defn touch [e]
  (walk/prewalk
   #(if (entity? %) (into {} %) %)
   e))

(comment
  (do
    (def conn (create))

    (transact! conn [[:db/add -1 :name "Foo"]
                     [:db/add -2 :friend (->ref [:name "Foo"])]]
               {:foo true})

    (datoms @conn)

    (:friend (->entity @conn 3)  )

    )

  (transact! conn [[:db/add -1 :name "Foo"]
                   [:db/add -1 :age 20]
                   [:db/add -2 :name "Bar"]])

  (transact! conn [[:db/add 2 :age 21]])

  (transact! conn [[:db/add 2 :friend (->ref 3)]])

  (transact! conn [{:name "Quux" :age 83}])

  (transact! conn [{:ast/term :application
                    :db/id -1
                    :function :concat
                    :arguments [{:ast/term :symbol :name "foo" :db/id -2}
                                {:ast/term :symbol :name "bar" :db/id -3}]}])

  (datoms @conn)

  (type (first (:arguments (->Entity @conn 9))))

  )

(def state (create))
