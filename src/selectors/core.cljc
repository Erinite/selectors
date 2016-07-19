(ns selectors.core
  (:require
    [com.rpl.specter :refer :all :exclude [view]]
    #? (:clj [com.rpl.specter.macros :as specter])
    [cljs.core.match :refer-macros  [match]])
  #?(:cljs (:require-macros [com.rpl.specter.macros :as specter])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Private implementation, move to selectors.impl

;; From hiccup.compiler:
(def ^{:doc "Regular expression that parses a CSS-style id and class from an element name."
      :private true}
    re-tag #"([^\s\.#]*)(?:#([^\s\.#]+))?(?:\.([^\s#]+))?")

(defn- parse-tag
  "Parse a keyword 'tag' into [tag attrs]"
  [tag]
  (let [[_ tag-name tag-id classes] (re-matches re-tag (name tag))
        tag-name                    (when (seq tag-name) (keyword tag-name))
        classes                     (set (when classes (filter seq (clojure.string/split classes #"\."))))]
    (vector
      tag-name 
      (cond
        (and tag-id classes) {:id tag-id :class classes}
        tag-id               {:id tag-id}
        classes              {:class classes}
        :else                {}))))

(specter/declarepath pre-order-walker [type match-fn?])
(specter/providepath pre-order-walker
  (if-path pred
    (if-path [pred]
      (stay-then-continue ALL (params-reset hiccup-walker))
      [ALL (params-reset hiccup-walker)])))

(specter/declarepath post-order-walker [type match-fn?])
(specter/providepath post-order-walker
  (if-path pred
    (if-path [pred]
      (continue-then-stay ALL (params-reset hiccup-walker))
      [ALL (params-reset hiccup-walker)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Public API

(defn view
  "Navigate to all nodes from which `path` successfully matches"
  [path]
  (post-order-walker vector? path))


(defn tag
  "Match if node has tag `tag-name`"
  [tag-name]
  #(= tag-name (first %)))


(defn sel-tag
  "Navigate to all nodes with a tag of `tag-name`"
  [tag-name]
  (view (tag tag-name)))


(defn id
  "Match if node has id `id-name`"
  [id-name]
  #(= id-name (:id (second %))))


(defn sel-id
  "Navigate to all nodes with `id` tag"
  [id-name]
  (view (id id-name)))


(defn classes
  "Match if node has all classes `class-names`"
  [class-names]
  (let [class-names (set class-names)]
    #(= class-names (clojure.set/intersection class-names (:class (second %))))))


(defn sel-classes
  "Navigate to all nodes with `class-names` classes"
  [class-names]
  (view (classes class-names)))


(defn attrs
  "Match if node has all attrs in `attrs`"
  [attr-map]
  #(= (select-keys (second %) (keys attr-map) attr-map)))


(defn sel-attrs
  "Navigate to all nodes with `attrs` attributes"
  [attr-map]
  (view (attrs attr-map)))


(defn content
  "Match if node's content matches `value`"
  [value]
  (if (or (number? value)
          (string? value))
    #(= value (last %))
    #(when (string? (last %)) (re-find (last %) value))))


(defn sel-content
  "Navigate to all nodes whose content matches `value`
  (equal if a string, matches if a regex)"
  [value]
  (view (content value)))


(defn sel
  "Navigate to all nodes matching `selector`"
  [selector]
  (mapv
    (fn [sel]
      (let [[t a]   (parse-tag sel)
            preds   (->>  (vector
                            (when t (tag t))
                            (when (:id a) (id (:id a)))
                            (when (:class a) (classes (:class a))))
                         (filter identity)) ]
        (view (apply every-pred preds))))
    selector))

(defn- normalize'
  [node]
  (let [[tag attrs children]  (if (map? (second node))
                                [(first node) (second node) (next node)]
                                [(first node) {} (next node)])]
    (let [[tag attribs] (parse-tag tag)]
      (apply vector tag (merge attrs attribs) children))))

(defn normalize
  "If called without any arguments, will return a multi-transform selector to normalize the hiccup.
  If called with a single hiccup structure as an argument, will return normalized hiccup."
  ([]
    [(post-order-walker vector? (constantly true)) (terminal normalize')])
  ([hiccup]
    (specter/multi-transform (multi-path (normalize)) hiccup)))
