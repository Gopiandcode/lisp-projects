(defstruct person
  	   name
	   age
	   waist-size
	   favorite-color)

(defparameter *bob* (make-person :name "Bob"
				 :age 35
				 :waist-size 32
				 :favorite-color "blue"))

(defparameter *that-guy* #S(person :name "Bob" :age 35 :waist-size 32 :favorite-color "blue"))

(princ #\newline)
(princ "length")
(princ (length '(a b c)))
(princ #\newline)
(princ "findif")
(princ (find-if #'numberp '(a b 5 d)))
(princ #\newline)
(princ "count")
(princ (count #\s "mississippi"))
(princ #\newline)
(princ "position")
(princ (position #\4 "2kewl4skewl"))
(arrayp 3)
(numberp 3)
(functionp 3)
(hash-table-p 3)
(listp 2)
(stringp 3)
(symbolp 3)

; old bad way
;(defun add (a b)
;  (cond ((and (numberp a) (numberp b)) (+ a b))
;	((and (listp a) (listp b)) (append a b))))

(defmethod add ((a number) (b number))
  (+ a b))
(defmethod add ((a list) (b list))
  (append a b))



