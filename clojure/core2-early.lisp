;;; -*- mode: clojure -*-
;;; This file is the bootstrapping inflection point -- it contains
;;; implementations of clojure.core functions written in Clojure.

(in-package "clojure.core")
(named-readtables:in-readtable cloture:cloture)

(defn empty? [xs]
  (not (seq xs)))
