;;;;visual graph

;; define 
(defparameter *wizard-nodes* '((living-room ( you are in the living-room.
				      a wizard is snoring loudly on the couch.))
			(garden (you are in a beautiful garden.
				 there is a well in front of you.))
			(attic (you are in the attic.
				there is a giant welding torch in the corner.))))
;; define edges , the path players can move 

(defparameter *wizard-edges* '( (living-room (garden west door)
			  (attic  upstairs ladder))
			(garden (living-room east door))
			(attic (living-room downstairs ladder))))
(defun dot-name(exp)
  (substitute-if #\_ (complement #'alphanumericp) (prin1-to-string exp)))

(defun dot-name2(exp)
  (substitute-if-not #\_  #'alphanumericp (prin1-to-string exp) ))

;;;next ,we will define the label, we will ensure the length of label no more than 30

;; so, 

(defparameter *max-label-length* 30)

(defun dot-label (exp)
  (if exp
      (let  (( s (write-to-string exp :pretty nil)))
	(if ( > (length s) *max-label-length*) 
	    (concatenate 'string (subseq s 0 (- *max-label-length* 3)) "...")
	    s))
      ""))

(defun nodes->dot (nodes )
  (mapc (lambda (node)
	  (fresh-line )
	  (princ (dot-name (car node)))
	  (princ "[label=\"")
	  (princ (dot-label node))
	  (princ "\"];"))
	nodes))
;;;define edges to dots

(defun edges->dot (edges )
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

;;; last
(defun graph->dot (nodes edges )
  (princ "digraph {")
  (nodes->dot nodes)
  (edges->dot edges)
  (princ "}"))



