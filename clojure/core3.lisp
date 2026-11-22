;;; -*- mode: clojure -*-
;;; clojure.core functions and macros Buboflash's shared code (bubrary-gui/src/buboflash/*.cljc)
;;; needs that Cloture had not implemented.  The functions are copied from ClojureScript's
;;; cljs/core.cljs (1.12.145), whose notice follows; where the original leans on a JS or
;;; transient facility Cloture lacks, the copy says what it does instead.
;;;
;;;   Copyright (c) Rich Hickey. All rights reserved.
;;;   The use and distribution terms for this software are covered by the
;;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;;   which can be found in the file epl-v10.html at the root of this distribution.
;;;   By using this software in any fashion, you are agreeing to be bound by
;;;   the terms of this license.
;;;   You must not remove this notice, or any other, from this software.

(in-package "clojure.core")
(named-readtables:in-readtable cloture:cloture)

(defn not-empty
  "If coll is empty, returns nil, else coll"
  [coll] (when (seq coll) coll))

(defn boolean
  "Coerce to boolean"
  [x]
  (cond
    (nil? x) false
    (false? x) false
    :else true))

(defn some?
  "Returns true if x is not nil, false otherwise."
  [x] (not (nil? x)))

(defn number?
  "Returns true if x is a number."
  [x] (CL:IF (CL:NUMBERP x) true false))

(defn coll?
  "Returns true if x satisfies ICollection: a vector, map, set or seq here."
  [x]
  (if (nil? x)
    false
    (or (vector? x) (map? x) (set? x) (seq? x))))

(defn some
  "Returns the first logical true value of (pred x) for any x in coll,
  else nil."
  [pred coll]
  (when-let [s (seq coll)]
    (or (pred (first s)) (recur pred (next s)))))

;; cljs reduces into a transient vector; a vector of the filtered seq is the same value
(defn filterv
  "Returns a vector of the items in coll for which
  (pred item) returns logical true. pred must be free of side-effects."
  [pred coll]
  (vec (filter pred coll)))

;; cljs conj!s onto a transient vector; a vector of the mapped seq is the same value
(defn mapv
  "Returns a vector consisting of the result of applying f to the
  set of first items of each coll, followed by applying f to the set
  of second items in each coll, until any one of the colls is
  exhausted."
  ([f coll] (vec (map f coll)))
  ([f c1 c2] (into [] (map f c1 c2)))
  ([f c1 c2 c3] (into [] (map f c1 c2 c3)))
  ([f c1 c2 c3 & colls] (into [] (apply map f c1 c2 c3 colls))))

;; the collection arity only; cljs's walks chunked seqs, the same values in the same order
(defn keep
  "Returns a lazy sequence of the non-nil results of (f item). Note,
  this means false return values will be included."
  [f coll]
  (remove nil? (map f coll)))

;; cljs's macro (cljs/core.cljc, from clojure/core.clj) partitions the clauses; Cloture has no
;; partition, so the pairs are taken by hand, and each step rebinds the one name
(defmacro cond->
  "Takes an expression and a set of test/form pairs. Threads expr (via ->)
  through each form for which the corresponding test
  expression is true."
  [expr & clauses]
  (let [g (gensym "cond")
        steps (loop [cs (seq clauses) acc []]
                (if cs
                  (recur (next (next cs))
                         (conj acc g (list 'if (first cs) (list '-> g (second cs)) g)))
                  acc))]
    (list 'let (into [g expr] steps) g)))

(defmacro some->
  "When expr is not nil, threads it into the first form (via ->),
  and when that result is not nil, through the next etc"
  [expr & forms]
  (let [g (gensym "some")
        steps (loop [fs (seq forms) acc []]
                (if fs
                  (recur (next fs)
                         (conj acc g (list 'if (list 'nil? g) nil (list '-> g (first fs)))))
                  acc))]
    (list 'let (into [g expr] steps) g)))
