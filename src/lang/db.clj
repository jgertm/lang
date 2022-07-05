(ns lang.db
  (:require [clojure.pprint]
            [clojure.walk :as walk]
            [lang.utils :refer [undefined]]))

(defrecord Ref
    [eid
     tempid
     lookup
     component?])

(comment
  (instance? clojure.lang.IRecord (->ref 2))

  (let [ref (->ref 'foo)]
    (= ref (reduce (fn [r x] (conj r x)) (->ref 0) ref)))

  (let [tempids {'foo 1}]
    (partial walk/postwalk #(if (and (ref? %) (contains? tempids (:tempid %)))
                              (assoc %
                                     :eid (get tempids (:tempid %))
                                     :tempid nil)
                              %) (->ref 'foo))))

(defmethod print-method Ref
  [{:keys [eid tempid lookup]} ^java.io.Writer w]
  (cond
    eid (.write w (format "#ref[%d]" eid))
    tempid (.write w (format "#temp[%s]" (pr-str tempid)))
    lookup (.write w (format "#lookup[%s]" (pr-str lookup)))))

(defmethod clojure.pprint/simple-dispatch Ref
  [^Ref ref]
  (pr ref))

(defn ->ref
  ([x] (->ref x false))
  ([x component?]
   (cond
     (pos-int? x)
     (map->Ref {:eid x :component? component?})

     (vector? x)
     (map->Ref {:lookup x :component? component?})

     :else
     (map->Ref {:tempid x :component? component?}))))

(defn ref? [x]
  (instance? Ref x))

(defrecord Datom
    [^clojure.lang.Keyword m
     ^long e
     ^clojure.lang.Keyword a
     v
     ^Ref tx])

(defmethod print-method Datom
  [{:keys [m e a v tx]} ^java.io.Writer w]
  (.write w (pr-str [m e a v (:eid tx)])))

(comment
  (Datom. :db/add 1 :foo "bar" 2)

  )

(defrecord DB [datoms])

(defn db? [x]
  (instance? DB x))

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
             (->ref (:db/id sub) true))
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
            (associative? fact))]
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

(defn next-txid
  [db]
  (->> db
       :datoms
       (map :e)
       (reduce max 0)
       inc))

(defn- reify-datoms
  [txid facts]
  (let [tempids
        (zipmap (->> facts
                     (map :e)
                     (filter #(and (not (pos-int? %)) (not (vector? %)) %))
                     distinct)
                (map (partial + txid 1) (range)))]
    {:tempids tempids
     :datoms
     (map
      (fn [fact]
        (-> fact
            (update :e #(get tempids % %))
            (update :v (partial walk/postwalk
                                (fn [node]
                                  (cond
                                    (and (ref? node) (contains? tempids (:tempid node)))
                                    (assoc node
                                           :eid (get tempids (:tempid node))
                                           :tempid nil)

                                    (and (ref? node) (:tempid node))
                                    (throw (ex-info "cannot resolve tempid"
                                                    {:fact fact
                                                     :ref node
                                                     :tempids tempids}))

                                    :else node))))
            (assoc :tx (map->Ref {:eid txid}))
            (map->Datom)))
      facts)}))

(defn- integrate
  [db datoms]
  (letfn [(into-indices [indices {:keys [m e a v tx] :as datom}]
            (case m
              :db/add (-> indices
                          (assoc-in [:eav e a] v)
                          (assoc-in [:ave a v] e)
                          (assoc-in [:eavt e a v tx] datom)
                          (assoc-in [:avet a v e tx] datom))))
          (into-datoms [datoms datom]
            (conj datoms datom))]
    (reduce
     (fn [acc {:keys [m e a v] :as datom}]
       (case m
         :db/add
         (let [old-v (get-in acc [:indices :eav e a] ::nf)]
           (if (not= old-v v)
             (-> acc
                 (update :indices into-indices datom)
                 (update :datoms into-datoms datom))
             acc))))
     (merge (select-keys db [:indices])
            {:datoms []})
     datoms)))

(defn with
  ([db facts] (with db facts nil))
  ([db facts tx-meta]
   {:pre [(db? db)]}
   (let [txid     (or (:db/id tx-meta) (next-txid db))
         {:keys [datoms tempids]}
         (->> facts
              (mapcat expand-fact)
              (reify-datoms txid))
         {:keys [datoms indices]}    (integrate db datoms)
         tx-datoms (map (fn [[k v]]
                          (map->Datom {:m :db/add :e txid :a k :v v :tx (map->Ref {:eid txid})})) tx-meta)]
     {:db-before db
      :db-after  (-> db
                     (update :datoms into (when (not-empty datoms) (concat tx-datoms datoms)))
                     (assoc :indices indices))
      :tx-data   datoms
      :tempids   tempids
      :tx-meta   tx-meta})))

(comment
  (with (init) [[:db/add -1 :foo "bar"]
                {:db/id 3 :baz :quux :name "Steve"}])

  )

(defn tx!
  ([conn facts] (tx! conn facts nil))
  ([conn facts tx-meta]
   (let [report (with @conn facts tx-meta)]
     (reset! conn (:db-after report))
     report)))

(defn datoms
  [db]
  (map (juxt :m :e :a :v #(-> % :tx :eid)) (:datoms db)))

(defn- get-latest
  [index k1 k2]
  (some->> (get-in index [k1 k2])
           (apply max-key #(->> % val keys (map :eid) (apply max)))
           key))

(defn- get-all
  [index k1 k2]
  (some->> (get-in index [k1 k2])
           keys))

(defn index
  [db order]
  {:pre [(db? db)]}
  (get (:indices db) order))

(defn dereference
  [db {:keys [eid tempid lookup] :as ref}]
  {:pre [(db? db) (ref? ref)]}
  (cond
    eid
    eid

    tempid
    (throw (ex-info "cannot dereference tempid" {:tempid tempid}))

    lookup
    (let [[a v] lookup]
      (get-in (index db :ave) [a v]))))

(declare ->Entity)

(deftype Entity
    [^DB db ^Ref ref]
  Object
  (toString [e] (pr-str (into {} (seq e))))

  clojure.lang.Seqable
  (seq [_]
    (let [eid (dereference db ref)
          avs (get (index db :eav) eid)]
      (some->> avs
               not-empty
               (cons [:db/id eid]))))

  clojure.lang.ILookup
  (valAt [this a]
    (let [v (get (into {} (.seq this)) a)]
      (cond
        (ref? v)
        (->Entity db v)

        (and (vector? v) (every? ref? v))
        (mapv (partial ->Entity db) v)

        :else v)))
  (valAt [this a not-found] (or (.valAt this a) not-found))

  clojure.lang.Associative
  (equiv [this other] (= (into {} (seq this))
                         (into {} (seq other))))
  (containsKey [this k] (not= ::nf (.valAt this k ::nf)))
  (entryAt [this k] (let [v (.valAt this k ::nf)]
                      (if (not= ::nf v)
                        (clojure.lang.MapEntry. k v)
                        nil)))

  (empty [_] (throw (UnsupportedOperationException.)))
  (assoc [this k v] (assoc (into {} (seq this)) k v))

  clojure.lang.IFn
  (invoke [this a] (.valAt this a))
  (invoke [this a not-found] (.valAt this a not-found)))

(defmethod print-method Entity
  [e ^java.io.Writer w]
  (.write w (str e)))

(defn entity? [x]
  (instance? Entity x))

(defn ->entity
  [db x]
  {:pre [(db? db)]}
  (if (ref? x)
    (->Entity db x)
    (->Entity db (->ref x))))

(defn touch [e]
  {:pre [(entity? e)]}
  (some->> e
       seq
       not-empty
       (into {})
       (walk/prewalk
        #(if (and (ref? %) (:component? %))
           (touch (->entity (.db e) %))
           %))))

(comment
  (do
    (def conn (create))

    (tx! conn
               [[:db/add -1 :name "Foo"]
                [:db/add -2 :friend (->ref [:name "Foo"] true)]]
               {:foo true})

    (datoms @conn)

    (touch (->entity @conn 3)))


  (tx! conn [[:db/add -1 :name "Foo"]
                   [:db/add -1 :age 20]
                   [:db/add -2 :name "Bar"]])

  (tx! conn [[:db/add 2 :age 21]])

  (tx! conn [[:db/add 2 :friend (->ref 3)]])

  (tx! conn [{:name "Quux" :age 83}])

  (tx! conn [{:ast/term  :application
              :db/id     -1
              :function  :concat
              :arguments [{:ast/term :symbol :name "foo" :db/id -2}
                          {:ast/term :symbol :name "bar" :db/id -3}]}])

  (datoms @conn)

  (type (first (:arguments (->Entity @conn 9))))

  )

(def state (create))
