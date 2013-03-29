;;; unicode.lisp

(in-package #:parsec)

(defun char-ascii-p (c)
  "Is character C within the ASCII set?"
  (< (char-code c) 128))

(defun char-latin1-p (c)
  "Is character C within the Latin-1 set?"
  (< (char-code c) 256))

(defun char-ascii-lower-p (c)
  "Is character C ASCII and lowercase?"
  (<= (char-code #\a) (char-code c) (char-code #\z)))

(defun char-ascii-upper-p (c)
  "Is character C ASCII and uppercase?"
  (<= (char-code #\A) (char-code c) (char-code #\Z)))

(defun char-control-p (c)
  "Is character C a control character?"
  (eql #\C (char (cl-unicode:general-category c) 0)))

(defun char-printable-p (c)
  "Is character C printable (not a control character)?"
  (not (char-control-p c)))

(defun char-space-p (c)
  "Is character C whitespace?"
  (member c '(#\Space #\Tab #\Newline #\Return #\Page #\No-Break_Space)))

(defun char-upper-p (c)
  "Is character C uppercase?"
  (equal (cl-unicode:general-category c) "Lu"))

(defun char-lower-p (c)
  "Is character C lowercase?"
  (equal (cl-unicode:general-category c) "Ll"))

(defun char-alpha-p (c)
  "Is character C alphabetic (same as CHAR-LETTER-P)?"
  (char-letter-p c))

(defun char-alphanumeric-p (c)
  "Is character C alphanumeric (same as ALPHANUMERICP)?"
  (alphanumericp c))

(defun char-digit-p (c)
  "Is character C a digit (0-9)?"
  (<= (char-code #\0) (char-code c) (char-code #\9)))

(defun char-oct-digit-p (c)
  "Is character C an octal digit (0-7)?"
  (<= (char-code #\0) (char-code c) (char-code #\7)))

(defun char-hex-digit-p (c)
  "Is character C hexadecimal (0-9, a-f, A-F)?"
  (or (char-digit-p c)
      (<= (char-code #\A) (char-code c) (char-code #\F))
      (<= (char-code #\a) (char-code c) (char-code #\f))))

(defun char-letter-p (c)
  "Is character C a letter?"
  (eql #\L (char (cl-unicode:general-category c) 0)))

(defun char-mark-p (c)
  "Is character C a mark?"
  (eql #\M (char (cl-unicode:general-category c) 0)))

(defun char-number-p (c)
  "Is character C a number?"
  (eql #\N (char (cl-unicode:general-category c) 0)))

(defun char-punctuation-p (c)
  "Is character C punctuation?"
  (eql #\P (char (cl-unicode:general-category c) 0)))

(defun char-symbol-p (c)
  "Is character C a symbol?"
  (eql #\S (char (cl-unicode:general-category c) 0)))

(defun char-separator-p (c)
  "Is character C a separator?"
  (eql #\Z (char (cl-unicode:general-category c) 0)))

