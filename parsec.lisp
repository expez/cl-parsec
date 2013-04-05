;;; parsec.lisp

(in-package #:parsec)

(defgeneric parse (parser input)
  (:documentation "Run a parser on some input value."))

(defmethod parse (parser (input string))
  (parse parser (make-instance 'input :contents input)))

(defmethod parse (parser (input input))
  (let ((*input* input))
    (values (funcall parser) *input*)))

(defmacro defparser (name args &body body)
  "Defines a parser to return a lambda, for lazy evaluation."
  `(defun ,name ,args (lambda () ,@body)))

(defun <- (&rest as)
  "A nicer alias for applying parsers."
  (apply #'funcall as))

(defun eof? ()
  "Are we at the end of the input?"
  (= (input-position *input*)
     (length (input-contents *input*))))

(defun bof? ()
  "Are we at the beginning of the input?"
  (zerop (input-position *input*)))

(defun next ()
  "Consume the next token from the input stream."
  (let ((tok (peek)))
    (incf (input-position *input*))
    tok))

(defun prev ()
  "Go back one token, returning its value."
  (if (bof?)
      (error 'bof-error)
      (progn
        (decf (input-position *input*))
        (peek))))

(defun peek ()
  "Look at the next character in the input, failing if EOF?"
  (if (eof?)
      (error 'eof-error)
      (char (input-contents *input*) (input-position *input*))))
