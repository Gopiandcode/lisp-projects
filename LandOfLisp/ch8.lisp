(load "ch7.lisp")

(defparameter *congestion-city-nodes* nil)
(defparameter *congestion-city-edges* nil)
(defparameter *visited-nodes* nil)
(defparameter *node-num* 30)
(defparameter *edge-num* 45)
(defparameter *worm-num* 3)
(defparameter *cop-odds* 15)

(defun random-node ()
  (1+ (random *node-num*)))

(defun edge-pair (a b)
  (unless (eql a b)
    (list (cons a b) (cons b a))))

(defun make-edge-list ()
  (apply #'append (loop repeat *edge-num*
			collect (edge-pair (random-node) (random-node)))))

(defun direct-edges (node edge-list)
  (remove-if-not (lambda (x)
		   (eql (car x) node))
		 edge-list))

(defun get-connected (node edge-list)
  (let ((visited nil))
    (labels ((traverse (node)
		       (unless (member node visited) ; if node is not visited
			 (push node visited)         ; add node to visited
			 (mapc (lambda (edge)        ; for each edge
				 (traverse (cdr edge)))  ; apply to children
			       (direct-edges node edge-list)))))   ; in the edges on the node
      (traverse node))
    visited)) ; return visited


(defun find-islands (nodes edge-list)
  (let ((islands nil))
    (labels ((find-island (nodes)
			  (let* ((connected (get-connected (car nodes) edge-list)) ; let connected be the conneded nodes
				 (unconnected (set-difference nodes connected)))   ; let unconnected be the remaining nodes
			    (push connected islands)                               ; add connected to islands
			    (when unconnected                                      ; if there are remaining nodes, recurse
			      (find-island unconnected)))))
      (find-island nodes))
    islands))

(defun connect-with-bridges (islands)
  (when (cdr islands) ; when there are remaining islands
    (append (edge-pair (caar islands) (caadr islands)) ; connect the first node of the current islands with the first nodes of the snd
	    (connect-with-bridges (cdr islands)))))    ; recurse on remaining

(defun connect-all-islands (nodes edge-list)
  (append (connect-with-bridges (find-islands nodes edge-list)) edge-list)) ; find islands then connect

(defun make-city-edges ()
  (let* ((nodes (loop for i from 1 to *node-num* 
		      collect i))     ; let nodes be a list of numbers 1 to nodenum
	 (edge-list (connect-all-islands nodes (make-edge-list))) ; let edge-list be a list of all edges
	 (cops (remove-if-not (lambda (x)                  
				(zerop (random *cop-odds*)))
			      edge-list)))                      ; let edges be a list where certain edges have been removed
    (add-cops (edges-to-alist edge-list) cops)))

(defun edges-to-alist (edge-list)
  (mapcar (lambda (node1)    ; for each first node in the list of edges
	    (cons node1
		  (mapcar (lambda (edge)        ; for each edge
			    (list (cdr edge)))  ; return a list containing the edge
			  (remove-duplicates (direct-edges node1 edge-list) ; with edges from the starting node
					     :test #'equal))))
	  (remove-duplicates (mapcar #'car edge-list)))) ; remove duplicates and convert edge list into a list continaing the first ed

(defun add-cops (edge-alist edges-with-cops)
  (mapcar (lambda (x)
	    (let ((node1 (car x))   ; let node be the first node in the edge
		  (node1-edges (cdr x))) ; let node1 be the edges
	      (cons node1           ; append node1 onto
		    (mapcar (lambda (edge)  ; generate a list by applying to each edge
			      (let ((node2 (car edge)))   ; node2 := first node in each edge
				(if (intersection (edge-pair node1 node2) ; if edges with cops contains the edge
						  edges-with-cops
						  :test #'equal)
				  (list node2 'cops)            ; if edge in copslist  return a list containing node2, cops
				  edge)))               ;  else return the edge
			    node1-edges))))  ; for each node in edges
	  edge-alist)) ; for each key in edge-association list

(defun neigbors (node edge-alist)
  (mapcar #'car (cdr (assoc node edge-alist))))

(defun within-one (a b edge-alist)
  (member b (neighbors a edge-alist)))

(defun  within-two (a b edge-alist)
  (or (within-one a b edge-alist)
      (some (lambda (x)
	      (within-one x b edge-alist))
	    (neighbors a edge-alist))))


(defun make-city-nodes (edge-alist)
  (let ((wumpus (random-node))
	(glow-worms (loop for i below *worm-num*
			  collect (random-node))))
    (loop for n from 1 to *node-num*
	  collect (append (list n)
			  (cond ((eql n wumpus) '(wumpus))
				((within-two n wumpus edge-alist) '(blood!)))
			  (cond ((member n glow-worms)
				 '(glow-worm))
				((some (lambda (worm)
					 (within-one n worm edge-alist))
				       glow-worms)
				 '(lights!)))
			  (when (some #'cdr (cdr (assoc n edge-alist)))
			    '(sirens!))))))

(defun find-empty-node ()
  (let ((x (random-node)))
  (if (cdr (assoc x *congestion-city-nodes*))
    (find-empty-node)
    x)))


(defun new-game ()
  (setf *congestion-city-edges* (make-city-edges))
  (setf *congestion-city-nodes* (make-city-nodes *congestion-city-edges*))
  (setf *player-pos* (find-empty-node))
  (setf *visited-nodes* (list *player-pos*))
  (draw-city)
  (draw-known-city))

(defun draw-city ()
  (ugraph->png "city" *congestion-city-nodes* *congestion-city-edges*))
(defun draw-known-city ()
  (ugraph->png "know-city" (known-city-nodes) (known-city-edges)))

(defun known-city-nodes ()
  (mapcar (lambda (node)
	    (if (member node *visited-nodes*)
	      (let ((n (assoc node *congestion-city-nodes*)))
		(if (eql node *player-pos*)
		  (append n '(*))
		  n))
	      (list node '?)))
	  (remove-duplicates
	    (append *visited-nodes*
		    (mapcan (lambda (node)
			      (mapcar #'car
				      (cdr (assoc node
						  *congestion-city-edges*))))
			    *visited-nodes*)))))
(defun known-city-edges ()
  (mapcar (lambda (node)
	    (cons node (mapcar (lambda (x)
				 (if (member (car x) *visited-nodes*) ; if visited
				   x                                  ; add node
				   (list (car x))))                   ; else add connected node
			       (cdr (assoc node *congestion-city-edges*))))) ; retrieve all connected nodes
	  *visited-nodes*))


(defun walk (pos)
  (handle-direction pos nil))
(defun charge (pos)
  (handle-direction pos t))


(defun handle-direction (pos charging)
  (let ((edge (assoc pos
		     (cdr (assoc *player-pos* *congestion-city-edges*)))))
    (if edge
      (handle-new-place edge pos charging)
      (princ "That location does not exist!"))))

(defun handle-new-place (edge pos charging)
  (let* ((node (assoc pos *congestion-city-nodes*))
	 (has-worm (and (member 'glow-worm node)
			(not (member pos *visited-nodes*)))))
    (pushnew pos *visited-nodes*)
    (setf *player-pos* pos)
    (draw-known-city)
    (cond ((member 'cop edge) (princ "You ran into the cops. Game over."))
	  ((member 'wumpus node) (if charging
				   (princ "You found the Wumpus!")
				   (princ "You ran into the Wumpus")))
	  (charging (princ "You wasted your last bullet. Game Over."))
	  (has-worm (let ((new-pos (random-node)))
		      (princ "You ran into a Glow Worm Gang! You're now at ")
		      (princ new-pos)
		      (handle-new-place nil new-pos nil))))))
