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
    (add-cops (edge-to-alist edge-list) cops)))


(defun edges-to-alist (edge-list)
  (mapcar (lambda (node1 )
	    (cons node1 
		  (mapcar (lambda (edge)
			    (list (cdr edge)))
			  (remove-duplicates (direct-edges node1 edge-list)
					     :test #'equal))))
	  (remove-duplicates (mapcar #'car edge-list))))