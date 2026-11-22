(in-package #:cloture)
(in-readtable clojure-shortcut)

(esrap:defrule replacement-string-special
    (and #\\ character)
  (:lambda (s)
    (esrap:text (second s))))
(esrap:defrule replacement-string-replacement
    (and #\$ (or #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))
  (:lambda (res)
    (1- (parse-integer (second res)))))
(esrap:defrule replacement-string-normal
    (+ (not (or #\\ #\$))))
(esrap:defrule replacement-string
    (+ (or replacement-string-special
         replacement-string-replacement
         replacement-string-normal))
  (:lambda (res)
    (loop for form in res
          collect (etypecase form
                    (list (coerce form 'string))
                    (string form)
                    (number form)))))

(defconstructor replacement-string
  (list list))

(defun parse-replacement-string (s)
  (replacement-string
   (esrap:parse 'replacement-string s)))

(defun-1 #_re-quote-replacement (s)
  (let ((parse (parse-replacement-string s)))
    (with-output-to-string (out)
      (dolist (elt (replacement-string-list parse))
        (etypecase elt
          (string (write-string elt out))
          (number (format out "\\$~a" (1+ elt))))))))

(defun-1 #_starts-with? (string prefix)
  (? (string^= prefix string)))

(defun-1 #_ends-with? (string suffix)
  (? (string$= suffix string)))

(defun join-items (coll)
  "COLL's items as strings, as Clojure's join stringifies them: any seqable -- a vector, a lazy
seq, a list -- and nil as no items.  (STRING-JOIN alone takes only a Lisp sequence, so a
vector or a lazy seq was a TYPE-ERROR.)"
  (if (falsy? coll)
      '()
      (iterate (for item in-seq coll)
        (collect (#_str item)))))

(defn #_join
  ((coll) (string-join (join-items coll) ""))
  ((sep coll) (string-join (join-items coll) (#_str sep))))

(defun replace-aux (s match replacement)
  (multiple-value-ematch (values match replacement)
    (((type character) (type character))
     (substitute match replacement s))
    (((type string) (type string))
     (string-replace-all match s replacement))
    (((type raw-regex) _)
     (replace-aux s
                  (ppcre:create-scanner
                   (raw-regex-string match))
                  replacement))
    (((type compiled-regex) _)
     (replace-aux s
                  (compiled-regex-function match)
                  replacement))
    ;; an empty replacement is no replacement string the grammar parses: it deletes the match
    (((type function) (and (type string) (satisfies emptyp)))
     (ppcre:regex-replace-all match s ""))
    (((type function) (type string))
     (replace-aux s match (parse-replacement-string replacement)))
    (((type function) (type replacement-string))
     (ppcre:regex-replace-all match s (replacement-string-list replacement)))))

;;; TODO Compiler macro for the regex and the replacement string.
(defun-1 |clojure.string|:|replace| (s match replacement)
  (replace-aux s match replacement))

(defun-1 |clojure.string|:|upper-case| (s)
  (check-type s string)
  (string-upcase s))

;;; Added for Buboflash's shared code.

(defun-1 |clojure.string|:|lower-case| (s)
  (check-type s string)
  (string-downcase s))

(defun-1 |clojure.string|:|includes?| (s substr)
  (? (search substr s)))

(defun-1 |clojure.string|:|split| (s re)
  ;; trailing empty strings dropped, as Clojure's and ClojureScript's split drop them (cl-ppcre's
  ;; split drops them too); the regex is cl-ppcre's, where `\s' is ASCII whitespace
  (convert 'seq (ppcre:split (raw-regex-string re) s)))
