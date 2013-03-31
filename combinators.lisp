;;; combinators.lisp

(in-package #:parsec)

(defparser choice (&rest ps)
  "Run each parser, stopping as soon as one succeeds.
Fails if a parser fails and consumes input, or if no choices succeed."
  (when (null ps)
    (error 'no-choices))

  (let ((before (input-position *input*)))
    (restart-case
        (handler-bind
            ((parsec-error (lambda (c)
                             (declare (ignore c))
                             (let ((after (input-position *input*)))
                               (when (= after before)
                                 (invoke-restart 'next-choice))))))
          (<- (car ps)))
      (next-choice ()
        (setf (input-position *input*) before)
        (<- (apply #'choice (cdr ps)))))))

(defparser replicate (n p)
  "Parse P, N times."
  (if (zerop n)
      '()
      (cons (<- p) (<- (replicate (- n 1) p)))))

(defparser between (before after p)
  "Run P wrapped by BEFORE and AFTER."
  (<- before)
  (let ((res (<- p)))
    (<- after)
    res))

(defparser option (d p)
  "Parse P, returning D if it fails."
  (let ((before (input-position *input*)))
    (restart-case
        (handler-bind
            ((parsec-error (lambda (c)
                             (declare (ignore c))
                             (let ((after (input-position *input*)))
                               (when (= after before)
                                 (invoke-restart 'use-default))))))
          (<- p))
      (use-default ()
        (setf (input-position *input*) before)
        d))))

(defparser optional (p)
  "Optionally parse P, returning NIL if it fails."
  (<- (option nil p)))

(defparser many-1 (p)
  "Parse 1 or more repetitions of a parser, stopping on failure."
  (let* ((result (<- p))
         (results (<- (many p))))
    (cons result results)))

(defparser skip-many-1 (p)
  "Parse one of more repetitions of P, ignoring the result."
  (<- (many-1 p))
  nil)

(defparser separate-by (delim p)
  "Parse zero or more of p, separated by delim."
  (handler-case
      (<- (separate-by-1 delim p))
    (parsec-error () '())))

(defparser separate-by-1 (delim p)
  "Parse at least one p, separated by delim."
  (let ((first (<- p))
        (rest (<- (many (chain delim p)))))
    (cons first rest)))

(defparser end-by (term p)
  "Parse zero or more of p, terminated by term."
  (handler-case
      (<- (end-by-1 term p))
    (parsec-error () '())))

(defparser end-by-1 (term p)
  "Parse at least one p, terminated by term."
  (let ((first (<- p)))
    (<- term)
    (cons first (<- (many (lambda ()
                            (let ((res (<- p)))
                              (<- term)
                              res)))))))

(defparser separate-end-by (delim p)
  "Parse zero or more P, separated and optionally terminated by DELIM."
  (handler-case
      (<- (separate-end-by-1 delim p))
    (parsec-error () '())))

(defparser separate-end-by-1 (delim p)
  "Parse at least one P, separated and optionally terminated by DELIM."
  (let ((x (<- p)))
    (<- (choice (chain delim
                       (lambda ()
                         (cons x (<- (separate-end-by delim p)))))
                (lambda () (list x))))))

(defparser lexeme (p)
  "Run a parser, skipping trailing whitespace."
  (let ((res (<- p)))
    (<- (many (satisfy #'char-space-p)))
    res))

(defparser segment (s)
  "Read a literal string, skipping trailing whitespace."
  (<- (lexeme (literal s))))

(defparser chain (&rest ps)
  "Run a succession of parsers."
  (let ((res (<- (car ps))))
    (if (null (cdr ps))
        res
        (<- (apply #'chain (cdr ps))))))

(defparser parens (p)
  "Run a parser wrapped in parentheses segments."
  (<- (between (segment "(") (segment ")") p)))

(defparser any-token ()
  "Read the next token from the input."
  (next))

(defparser not-followed-by (p)
  "Only succeed if P fails."
  (<- (try (choice
            (lambda ()
              (let ((res (<- (try p))))
                (format t "result: ~s~%" res)
                (error 'unexpected :got res)))
            (lambda () nil)))))

(defparser eof ()
  "Parse the end of the input."
  (<- (not-followed-by (any-token))))

(defparser look-ahead (p)
  "Parse P without updating the position or state."
  (let* ((start-pos (input-position *input*))
         (start-state (input-state *input*))
         (res (<- p)))
    (setf (input-position *input*) start-pos)
    (setf (input-state *input*) start-state)
    res))

(defparser many-till (p end)
  "Parse many repetitions of P until END succeeds."
  (<- (choice (chain end (lambda () '()))
              (lambda ()
                (let ((x (<- p))
                      (xs (<- (many-till p end))))
                  (cons x xs))))))
