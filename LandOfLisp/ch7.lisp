;;;; set print circle to avoid printing errors
(setf *print-circle* t)

(defparameter *foo* '(1 2 3))

(setf (cdddr *foo*) *foo*)


(defparameter *drink-order* '((bill . double-expresso)
			      (lisa . small-drip-coffee)
			      (john . medium-latte)
			     )
)

(defparameter *house* '((walls (mortar (cement)
				       (water)
				       (sand))
			       (bricks))
			(windows (glass)
				 (frame)
				 (curtains))
			(roof (shingles)
			      (chimney))))

(defun dot-name (exp)
  (substitute-if #\_ (complement #'alphanumericp) (prin1-to-string exp)))

(defparameter *max-label-length* 30)

(defun dot-label (exp)
  (if exp
    (let ((s (write-to-string exp :pretty nil)))
      (if (> (length s) *max-label-length*)
	(concatenate 'string (subseq s 0 (- *max-label-length* 3)) "...")
	s))
    ""))

(defun nodes->dot (nodes)
  (mapc (lambda (node)
	  (fresh-line)
	  (princ (dot-name (car node)))
	  (princ "[label=\"")
	  (princ (dot-label node))
	  (princ "\"];"))
	nodes))

(defun edges->dot (edges)
  (mapc (lambda (node)
	  (mapc (lambda (edge)
		  (fresh-line)
		  (princ (dot-name (car node)))
		  (princ "->")
		  (princ (dot-name (car edge)))
		  (princ "[label=\"")
		  (princ (dot-label (cdr edge)))
		  (princ "\"];"))
		(cdr node)))
	  edges))

(defun graph->dot (nodes edges)
  (princ "digraph{")
  (nodes->dot nodes)
  (edges->dot edges)
  (princ "}"))

(defun dot->png (fname thunk)
  (with-open-file (*standard-output*
		    fname
		    :direction :output
		    :if-exists :supersede)
    (funcall thunk))
  (ext:shell (concatenate 'string "dot -Tpng -O " fname)))