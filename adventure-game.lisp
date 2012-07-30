;;;; adventure game in on land of lisp
;;;; editor: jonny wei
;;;; date  : 2012-07-28
;;;;

;; define nodes
(defparameter *nodes* '((living-room ( you are in the living-room.
				      a wizard is snoring loudly on the couch.))
			(garden (you are in a beautiful garden.
				 there is a well in front of you.))
			(attic (you are in the attic.
				there is a giant welding torch in the corner.))))
;; describe location
(defun describe-location (location nodes)
  (cadr (assoc location nodes)))


;; define edges , the path players can move 

(defparameter *edges* '( (living-room (garden west door)
			  (attic  upstairs ladder))
			(garden (living-room east door))
			(attic (living-room downstairs ladder))))

;; describe-path fuction 

(defun describe-path (edge)
  `(there is a ,(caddr edge) going ,(cadr edge) from here.))

;; describe all paths
(defun describe-paths (location edges)
  (apply #'append (mapcar #'describe-path (cdr (assoc location edges)))))

;;define objects
(defparameter *objects* '(whiskey bucket frog chain))

;;define objects location 
(defparameter  *object-locations* '((whiskey living-room  )
				    (bucket living-room)
				    (chain garden)
				    (frog garden)))


;;list objects in some locations

(defun objects-at (location objs objs-locs)
  (labels ( (at-loc-p (obj)
	      (eq ( cadr (assoc obj objs-locs)) location)))
    (remove-if-not #'at-loc-p objs)))


;;describing visible objects
(defun describe-objects (loc objs obj-loc)
  (labels ((describe-obj(obj)
	     `(you see a ,obj on the floor.)))
    (apply #'append (mapcar #'describe-obj (objects-at loc objs obj-loc)))))

;;a variable track plaer's current position

(defparameter *location* 'living-room)

(defun look ()
  (append (describe-location *location* *nodes*)
	  (describe-paths *location* *edges*)
	  (describe-objects *location* *objects* *object-locations*)))

;; walk function
(defun walk (direction )
  (let ((next (find direction 
		    (cdr (assoc *location* *edges*))
		    :key #'cadr)))
   (if next
    (progn (setf *location* (car next))
	   (look))
    '(you cannot go that way.))))


;;;pick up objects 

(defun pickup (object)
  (cond ((member object (objects-at *location* *objects* *object-locations*))
	 (push (list object 'body) *object-locations*)
	 `(you are now carring the ,object))
	(t '(you cannot get that.))))

;;;checking our inventory
(defun inventory()
  (cons 'items- (objects-at 'body *objects* *object-locations*)))

;;define game repl
;; test 
(defun game-repl1()
  (loop (print (eval (read )))))
;; normal game repl
(defun game-repl ()
  (let ((cmd (game-read)))
    (unless (eq (car cmd) 'quit)
      (game-print (game-eval cmd))
      (game-repl))))


;;define game read
(defun game-read ()
  (let ((cmd (read-from-string 
	      (concatenate 'string "(" (read-line) ")" ))))
    (flet ( (quote-it (object)
	  (list 'quote object)))
      (cons (car cmd) (mapcar #'quote-it (cdr cmd) ) ))))
   
   ; (append 'string "(" (read-line) ")" )))
   ; append only be used for list


;;;define game eval

;; first define the function that the game can eval

(defparameter *allowed-commands* '(look walk pickup inventory))

;;then define game eval
(defun  game-eval (expr)
  (if (member (car expr ) *allowed-commands* )
      (eval expr)
      '(i do not know the command.)))

;; define simplest game-print

(defun game-print (lst)
  (princ lst))