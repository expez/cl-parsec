;;; test.lisp
(defpackage #:parsec-tests
    (:use #:cl #:parsec #:fiveam))

(in-package #:parsec-tests)

(def-suite parsec-tests)
(in-suite parsec-tests)

(defmacro with-parse (parser input rname iname &body body)
  `(multiple-value-bind (,rname ,iname) (parse ,parser ,input)
     ,@body))

(defmacro failing-parse (parser input &rest handlers)
  `(handler-case
       (progn
         (parse ,parser ,input)
         (fail "parser ~s expected to fail with input ~s" ',parser ,input))
     ,@handlers))

(test any-token
  (with-parse (any-token) "abc" res inp
    (is (eql #\a res))
    (is (= 1 (input-position inp)))))

(test try
  (with-parse (try (literal "foo")) "foobar" res inp
    (is (equal "foo" res))
    (is (= 3 (input-position inp))))

  (failing-parse (try (literal "foo")) "foul"
                 (unexpected (c) (is (= 0 (input-position (parsec-error-input c)))))))

(test many
  (with-parse (many (token #\a)) "aaabbb" res inp
    (is (equal '(#\a #\a #\a) res))
    (is (= 3 (input-position inp))))

  (with-parse (many (token #\a)) "bbbaaa" res inp
    (is (null res))
    (is (= 0 (input-position inp)))))

(test choice
  (with-parse (choice (literal "foo") (literal "bar")) "bar" res inp
    (is (equal "bar" res))
    (is (= 3 (input-position inp))))

  (with-parse (choice (try (literal "foo")) (literal "fizz")) "fizz" res inp
    (is (equal "fizz" res))
    (is (= 4 (input-position inp))))

  (failing-parse (choice (literal "bar")) "foo"
                 (no-choices () t))

  (failing-parse (choice (literal "foo") (literal "fizz")) "fizz"
                 (unexpected (u)
                             (is (eql #\o (unexpected-wanted u)))
                             (is (eql #\i (unexpected-got u))))))

(test replicate
  (with-parse (replicate 4 (token #\a)) "aaaab" res inp
    (is (equal `(#\a #\a #\a #\a) res))
    (is (= 4 (input-position inp))))

  (failing-parse (replicate 4 (token #\a)) "aaab"
                 (unexpected (u)
                             (is (eql #\a (unexpected-wanted u)))
                             (is (eql #\b (unexpected-got u))))))

(test between
  (with-parse (between (literal "(") (literal ")") (literal "foo")) "(foo)" res inp
    (is (equal "foo" res))
    (is (= 5 (input-position inp))))

  (failing-parse (between (literal "(") (literal ")") (literal "foo")) "(foo"
                 (eof-error (u)
                            (is (= 4 (input-position (parsec-error-input u)))))))

(test option
  (with-parse (option "hi" (literal "argh")) "argh" res inp
    (is (equal "argh" res))
    (is (= 4 (input-position inp)))

    (with-parse (option "hi" (literal "foo")) "argh" res inp
      (is (equal "hi" res))
      (is (= 0 (input-position inp))))))

(run!)
