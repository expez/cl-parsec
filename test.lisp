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
                 (unexpected (c) (is (= 0 (input-position
                                           (parsec-error-input c)))))))

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
  (with-parse (between (literal "(") (literal ")")
                       (literal "foo")) "(foo)" res inp
    (is (equal "foo" res))
    (is (= 5 (input-position inp))))

  (failing-parse (between (literal "(") (literal ")") (literal "foo")) "(foo"
                 (eof-error (u)
                            (is (= 4 (input-position
                                      (parsec-error-input u)))))))

(test option
  (with-parse (option "hi" (literal "argh")) "argh" res inp
    (is (equal "argh" res))
    (is (= 4 (input-position inp)))

    (with-parse (option "hi" (literal "foo")) "argh" res inp
      (is (equal "hi" res))
      (is (= 0 (input-position inp))))))

(test optional
  (with-parse (optional (literal "foo")) "foobar" res inp
    (is (equal "foo" res))
    (is (= 3 (input-position inp)))

    (with-parse (optional (literal "asdf")) "foobar" res inp
      (is (null res))
      (is (= 0 (input-position inp))))))

(test many-1
  (with-parse (many-1 (literal "a")) "aaasdf" res inp
    (is (equal '("a" "a" "a") res))
    (is (= 3 (input-position inp)))

    (failing-parse (many-1 (literal "a")) "sdf"
                   (unexpected (u)
                               (is (eql #\a (unexpected-wanted u)))
                               (is (eql #\s (unexpected-got u)))
                               (is (= 0 (input-position
                                         (parsec-error-input u))))))))

(test skip-many-1
  (with-parse (skip-many-1 (token #\a)) "aaasdf" res inp
    (is (null res))
    (is (= 3 (input-position inp))))

  (failing-parse (skip-many-1 (literal "a")) "sdf"
                 (unexpected (u)
                             (is (equal #\a (unexpected-wanted u)))
                             (is (equal #\s (unexpected-got u)))
                             (is (= 0 (input-position
                                       (parsec-error-input u)))))))

(test separate-by-1
  (with-parse (separate-by-1 (token #\:) (letter)) "f:b:a" res inp
    (is (equal '(#\f #\b #\a) res))
    (is (= 5 (input-position inp))))

  (failing-parse (separate-by-1 (token #\: ) (literal "f")) ":ad"
                 (unexpected (u)
                             (is (eql #\: (unexpected-got u)))
                             (is (= 0 (input-position
                                       (parsec-error-input u)))))))

(test separate-by
  (with-parse (separate-by (token #\:) (literal "foo")) "" res inp
    (is (equal nil res))
    (is (= 0 (input-position inp))))
  (with-parse (separate-by (token #\:) (literal "foo")) "foo:foo" res inp
    (is (equal '("foo" "foo") res))
    (is (= 7 (input-position inp)))))

(test end-by-1
  (with-parse (end-by-1 (token #\]) (literal "foo")) "foo]" res inp
    (is (equal '("foo") res))
    (is (= 4 (input-position inp))))
  (with-parse (end-by-1 (token #\]) (literal "foo")) "foo]foo]" res inp
    (is (equal '("foo" "foo") res))
    (is (= 8 (input-position inp))))
  (failing-parse (end-by-1 (token #\]) (literal "f")) ":ad"
                 (unexpected (u)
                             (is (eql #\: (unexpected-got u)))
                             (is (= 0 (input-position
                                       (parsec-error-input u)))))))

(test end-by
  (with-parse (end-by (token #\]) (literal "foo")) "foo]" res inp
    (is (equal '("foo") res))
    (is (= 4 (input-position inp))))
  (with-parse (end-by (token #\]) (literal "foo")) "foo]foo]" res inp
    (is (equal '("foo" "foo") res))
    (is (= 8 (input-position inp))))
  (with-parse (end-by (token #\]) (literal "foo")) "asdf" res inp
    (is (equal nil res))
    (is (= 0 (input-position inp)))))

(test separate-end-by-1
  (with-parse (separate-end-by-1 (token #\]) (literal "foo")) "foo]" res inp
    (is (equal '("foo") res))
    (is (= 4 (input-position inp))))
  (with-parse (separate-end-by-1 (token #\]) (literal "foo")) "foo" res inp
    (is (equal '("foo") res))
    (is (= 3 (input-position inp))))
  (failing-parse (separate-end-by-1 (token #\]) (literal "f")) ":ad"
                 (unexpected (u)
                             (is (eql #\: (unexpected-got u)))
                             (is (= 0 (input-position
                                       (parsec-error-input u)))))))

(test separate-end-by
  (with-parse (separate-end-by (token #\]) (literal "foo")) "foo]" res inp
    (is (equal '("foo") res))
    (is (= 4 (input-position inp))))
  (with-parse (separate-end-by (token #\]) (literal "foo")) "foo]foo" res inp
    (is (equal '("foo" "foo") res))
    (is (= 7 (input-position inp)))))

(test lexeme
  (with-parse (lexeme (literal "foo")) "foo  " res inp
    (is (equal "foo" res))
    (is (= 5 (input-position inp))))
  (with-parse (lexeme (literal "foo")) "foo" res inp
    (is (equal "foo" res))
    (is (= 3 (input-position inp))))
  (failing-parse (lexeme (literal "foo")) " foo"
                 (unexpected (u)
                             (is (eql #\Space (unexpected-got u)))
                             (is (= 0 (input-position
                                       (parsec-error-input u)))))))

(test segment
  (with-parse (segment  "foo") "foo bar" res inp
    (is (equal "foo" res))
    (is (= 4 (input-position inp))))
  (with-parse (segment "foo") "foo  " res inp
    (is (equal "foo" res))
    (is (= 5 (input-position inp))))
  (failing-parse (segment "foo") ":ad"
                 (unexpected (u)
                             (is (eql #\: (unexpected-got u)))
                             (is (= 0 (input-position
                                       (parsec-error-input u)))))))

(test chain
  (with-parse (chain (segment  "foo") (lexeme (literal "bar"))) "foo bar" res inp
    (is (equal "bar" res))
    (is (= 7 (input-position inp))))
  (failing-parse (chain (segment "foo") (lexeme (literal "bar"))) ":ad"
                 (unexpected (u)
                             (is (eql #\: (unexpected-got u)))
                             (is (= 0 (input-position
                                       (parsec-error-input u)))))))

(test parens
  (with-parse (parens (literal "foo")) "(foo) bar" res inp
    (is (equal "foo" res))
    (is (= 6 (input-position inp))))
  (failing-parse (parens (literal "foo")) "foo"
                 (unexpected (u)
                             (is (eql #\f (unexpected-got u)))
                             (is (eql #\( (unexpected-wanted u)))
                             (is (= 0 (input-position
                                       (parsec-error-input u)))))))

(test not-followed-by
  (with-parse (not-followed-by (literal ".")) "foo bar." res inp
    (is (null res))
    (is (= 0 (input-position inp))))
  (failing-parse (not-followed-by (literal ".")) ".foo"
                 (unexpected (u)
                             (is (equal "." (unexpected-got u)))
                             (is (= 0 (input-position
                                       (parsec-error-input u)))))))

(test eof
  (with-parse (eof) "" res inp
    (is (null res))
    (is (= 0 (input-position inp))))
  (failing-parse (eof) "foo"
                 (unexpected (u)
                             (is (eq #\f (unexpected-got u)))
                             (is (= 0 (input-position
                                       (parsec-error-input u)))))))

(test look-ahead
  (with-parse (look-ahead (literal "foo")) "foo" res inp
    (is (equal "foo" res))
    (is (= 0 (input-position inp))))
  (failing-parse (look-ahead (literal "foo")) "bar "
                 (unexpected (u)
                             (is (eq #\b (unexpected-got u)))
                             (is (= 0 (input-position
                                       (parsec-error-input u)))))))

(test many-till
  (with-parse (many-till (literal "f") (literal "$")) "ffff$" res inp
    (is (equal '("f" "f" "f" "f") res))
    (is (= 5 (input-position inp))))
  (failing-parse (many-till (literal "f") (literal "$")) "bar"
                 (error () nil)))
(run!)
