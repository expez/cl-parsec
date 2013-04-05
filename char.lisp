;;; char.lisp

(in-package #:parsec)

(defparser satisfy (pred)
  "Parse the next token only if it satisfies a given predicate."
  (let ((tok (next)))
    (if (funcall pred tok)
        tok
        (progn
          (prev)
          (error 'did-not-satisfy :token tok :predicate pred)))))

(defparser token (x)
  "Read a given character from the input, failing if it's not a match."
  (handler-case
      (<- (satisfy (lambda (y) (char= x y))))
    (did-not-satisfy (y)
      (error 'unexpected :wanted x :got (did-not-satisfy-token y)))))

(defparser one-of (chars)
  "Parses successfully if the next character is in the given list."
  (<- (satisfy (lambda (c) (member c chars)))))

(defparser none-of (chars)
  "Parses successfully if the next character is NOT in the given list."
  (<- (satisfy (lambda (c) (not (member c chars))))))

(defparser spaces ()
  "Skips over whitespace."
  (<- (skip-many (spacet))))

(defparser spacet ()
  "Skips one whitespace character."
  (<- (satisfy #'char-space-p)))

(defparser newline ()
  "Parses a newline character."
  (<- (token #\Newline)))

(defparser tab ()
  "Parses a tab character."
  (<- (token #\Tab)))

(defparser any-char ()
  "Parses the next character."
  (next))

(defparser literal (s)
  "Read a literal string from the input."
  (loop
     for c across s
     do (<- (token c)))
  s)

(defparser alphanumeric ()
  "Parses an alphanumeric character."
  (<- (satisfy #'char-alphanumeric-p)))

(defparser upper ()
  "Parses an uppercase character."
  (<- (satisfy #'char-upper-p)))

(defparser lower ()
  "Parses a lowercase character."
  (<- (satisfy #'char-lower-p)))

(defparser letter ()
  "Parses an uppercase or lowercase character."
  (<- (satisfy #'char-letter-p)))

(defparser digit ()
  "Parses a numeric character (0-9)."
  (<- (satisfy #'char-digit-p)))

(defparser oct-digit ()
  "Parses an octal character (0-7)."
  (<- (satisfy #'char-oct-digit-p)))

(defparser hex-digit ()
  "Parses a hexadecimal character (0-9, a-f, A-F)."
  (<- (satisfy #'char-hex-digit-p)))
