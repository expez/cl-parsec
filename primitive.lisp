;;; primitive.lisp

(in-package #:parsec)

(defparser try (p)
  "Try running a parser, resetting the position if it fails."
  (let ((start (input-position *input*)))
    (handler-bind
        ((parsec-error #'(lambda (c)
                           (declare (ignore c))
                           (setf (input-position *input*) start))))
      (<- p))))

(defparser many (p)
  "Parse zero or more repetitions of a parser, stopping on failure."
  (handler-case
      (let* ((before (input-position *input*))
             (result (<- p))
             (after (input-position *input*)))
        (if (= before after)
            (error 'many-unconsumed)
            (let ((rest (<- (many p))))
              (cons result rest))))
    (parsec-error () '())))

(defparser skip-many (p)
  "Parse zero or more repetitions of a parser, stopping on failure and skipping the result."
  (<- (many p))
  nil)
