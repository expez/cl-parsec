;;; data.lisp

(in-package #:parsec)

(defvar *input*
  "Current input state.")

;; input structure
(defclass input ()
  ((contents
    :initarg :contents
    :initform 0
    :accessor input-contents)
   (state
    :initarg :state
    :initform nil
    :accessor input-state)
   (position
    :initform 0
    :accessor input-position)))


;; conditions

;; generic parsing error
(define-condition parsec-error (error)
  ((input
    :initarg :input
    :initform *input*
    :reader parsec-error-input)
   (text
    :initarg :text
    :reader parsec-error-text)))

;; reached EOF
(define-condition eof-error (parsec-error) ())

;; trying to go back too far
(define-condition bof-error (parsec-error) ())

;; no choices succeeded
(define-condition no-choices (parsec-error) ())

;; parser accepting empty input passed to MANY
(define-condition many-unconsumed (parsec-error) ())

;; satisfy combinator failure
(define-condition did-not-satisfy (parsec-error)
  ((token
    :initarg :token
    :reader did-not-satisfy-token)
   (predicate
    :initarg :predicate
    :reader did-not-satisfy-predicate)))

;; unexpected token
(define-condition unexpected (parsec-error)
  ((wanted
    :initarg :wanted
    :initform nil
    :reader unexpected-wanted)
   (got
    :initarg :got
    :reader unexpected-got)))


;; pretty-printing
(defmethod print-object ((p parsec-error) s)
  (format s "parsec error: ~a" (parsec-error-text p)))

(defmethod print-object ((i input) s)
  (print-unreadable-object (i s :type t)
    (format s "~s @ ~a~@[ [~s]~]"
            (input-contents i)
            (input-position i)
            (input-state i))))

(defmethod print-object ((u unexpected) s)
  (format s "unexpected ~s~@[, expecting ~s~]"
          (unexpected-got u)
          (unexpected-wanted u)))

(defmethod print-object ((e eof-error) s)
  (format s "unexpected end of input"))

(defmethod print-object ((e bof-error) s)
  (format s "unexpected beginning of input"))

(defmethod print-object ((e no-choices) s)
  (format s "no parsers in CHOICE succeeded"))

(defmethod print-object ((e many-unconsumed) s)
  (format s "parser used with MANY accepts empty input"))

(defmethod print-object ((d did-not-satisfy) s)
  (format s "token ~s did not satisfy predicate ~s"
          (did-not-satisfy-token d)
          (did-not-satisfy-predicate d)))