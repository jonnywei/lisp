;;;; hunt the wumpus 


;;global define parameter 

(defparameter *congestion-city-nodes* nil)
(defparameter *congestion-city-edges* nil)
(defparameter *visited-nodes* nil )
(defparameter *node-num* 30)
(defparameter *edge-num* 45)
(defparameter *worm-num* 3 )
(defparameter *cops-odds* 15 )

;;; Generating Random edges

;;
(defun random-node ()
  (1+ (random *node-num*)))

;;
(defun edge-pair (a b)
  (unless (eq a b)
    (list (cons a b ) (cons b a))))

;;
(defun make-edge-list ()
  (apply #'append (loop repeat *edge-num*
		       collect (edge-pair (random-node) (random-node)))))


;;;prevent islands

;;return direct link nodes

(defun direct-edges (node edge-list)
  (remove-if-not (lambda (x) 
		   (eql (car x) node))
		 edge-list))
;; this function return all connected node
(defun get-connected (node edge-list)
  (let ((visited nil))
    (labels ((traverse (node)
	       (unless (member node visited)
		 (push node visited)
		 (mapc (lambda (edge)
			 (traverse (cdr edge)))
		       (direct-edges node edge-list)))))
      (traverse node))
    visited))


;;find all islands
(defun find-islands (nodes edge-list)
  (let ((islands nil))
    (labels ((find-island (nodes)
	       (let* (( connected (get-connected (car nodes) edge-list))
		      (unconnected (set-difference nodes connected)))
		 (push connected islands)
		 (when unconnected 
		   (find-island unconnected)))))
      (find-island nodes))
    islands))
	    

;; connect with bridges

(defun connect-with-bridges ( islands )
  (when (cdr islands) ;  at least two islands ,if have one island, then all nodes be  connected
    (append (edge-pair ( caar islands ) (caadr islands ))
	    (connect-with-bridges (cdr islands )))))

;; connectd all islands 
(defun connect-all-islands (nodes edge-list)
  (append (connect-with-bridges (find-islands  nodes edge-list)) edge-list))


;;; make congistion city edges

;; 
(defun make-city-edges ()
  (let* ((nodes (loop for i from 1 to *node-num* 
		     collect i))
	 (edge-list (connect-all-islands nodes (make-edge-list)))
	 (cops (remove-if-not (lambda (x)
				(zerop (random *cops-odds*))) ;1/*cop-odds* appear 0, so random ratio is 
			      edge-list)))
    (add-cops (edges-to-alist edge-list) cops)))


(defun edges-to-alist (edge-list)
  (mapcar (lambda (node1 )
	    (cons node1 
		  (mapcar (lambda (edge)
			    (list (cdr edge)))
			  (remove-duplicates (direct-edges node1 edge-list)
					     :test #'equal))))
	  (remove-duplicates (mapcar #'car edge-list))))

;; 
(defun add-cops (edge-list edges-with-cops)
  (mapcar (lambda (x)
	    (let (( node1 (car x))
		  ( node1-edges (cdr x)))
	      (cons node1 
		    (mapcar (lambda (edge)
			      (let ((node2 (car edge)))
				(if (intersection (edge-pair node1 node2)
						  edges-with-cops
						  :test #'equal)
				    (list node2 'cops)
				    edge)))
			    node1-edges))))
	  edge-list))

;;; build city nodes

(defun neighbors (node edge-list)
  (mapcar  #'car (cdr ( assoc node edge-list))))

;;define within one
(defun within-one (a b edge-list)
  (member b ( neighbors a edge-list)))

;;define within two
(defun within-two ( a b edge-alist)
  (or (within-one a b edge-alist)
      (some (lambda (x )
	      (within-one x b edge-alist))
	    (neighbors a edge-alist))))

;;make city nodes
(defun make-city-nodes (edge-alist)
  (let ((wumpus (random-node))
	(glow-worms (loop for i below *worm-num*
			 collect(random-node))))
    (loop for n from 1 to *node-num*
	 collect (append (list n)
			 (cond ((eql n wumpus) '(wumpus))
			       ((within-two n  wumpus edge-alist) '(blood!)))
			 (cond ((member n glow-worms)
				'(glow-worm))
			       (( some (lambda (worm) (within-one n worm edge-alist)) glow-worms)
				'(lights!)))
			 (when (some #'cdr (cdr (assoc n edge-alist)))
			   '(sirens!))))))

;; find empty node

(defun find-empty-node ()
  (let ((x  (random-node)))
    (if (cdr (assoc x *congestion-city-nodes* ))
	(find-empty-node )
	x)))


(defun new-game ()
  (setf *congestion-city-edges* (make-city-edges))
  (setf *congestion-city-nodes* (make-city-nodes *congestion-city-edges*))
  (setf *player-pos* (find-empty-node))
  (setf *visited-nodes* (list *player-pos*))
  (draw-city))
   

(load "/home/wjj/work/lisp/visual-graph.lisp")

(defun draw-city()
  (graph->dot  *congestion-city-nodes* *congestion-city-edges*))



