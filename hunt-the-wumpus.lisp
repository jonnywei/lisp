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
