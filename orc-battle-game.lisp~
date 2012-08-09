;;;; orc battle game
;;;;


;;; step 1 define global variables

;;
(defparameter *player-health* nil)
(defparameter *player-agility* nil)
(defparameter *player-strength* nil)


;;
(defparameter *monsters* nil)
(defparameter *monster-builders* nil) ;contains  monster builder function 
(defparameter *monster-num* 12)

;;;main game

(defun orc-battle()
  (init-monsters)
  (init-player)
  (game-loop)
  (when (player-dead)
    (princ "You have been killed.Game over."))
  (when (monsters-dead)
    (princ "Congratulations! You have vanquished all of your foes." )))


(defun game-loop ()
  (unless (or ( player-dead)  (monsters-dead))
    (show-player)
    (dotimes (k (1+ (truncate (/ (max 0 *player-agility*) 15))))
      (unless (monsters-dead)
	(show-monsters)
	(player-attack)))
    (fresh-line)
    (map 'list (lambda (m)
		 (or (monster-dead m) (monster-attack m)))
	 *monsters*)
    (game-loop)))


;;;player management functions

(defun init-player ()
  (setf *player-health* 30)
  (setf *player-agility* 30)
  (setf *player-strength* 30))

(defun player-dead ()
  (<= *player-health* 0))

(defun show-player ()
  (fresh-line)
  (princ "You are a valiant knight with a health of ")
  (princ *player-health* )
  (princ ",an agility of ")
  (princ *player-agility* )
  (princ ", an a strength of ")
  (princ *player-strength* ))


(defun player-attack ()
  (fresh-line)
  (princ "Attack style: [s]tab [d]ouble swing [r]oundhouse:")
  (case (read)
    (s (monster-hit (pick-monster)
		    (+ 2 (randval (ash *player-strength* -1)))))
    (d (let ((x (randval (truncate (/ *player-strength* 6)))))
	 (princ "You double swing has a strength of ")
	 (princ x)
	 (fresh-line)
	 (monster-hit (pick-monster) x)
	 (unless (monsters-dead)
	   (monster-hit (pick-monster) x ))))
    (otherwise ( dotimes (x (1+ (randval (truncate (/ *player-strength* 3)))))
		  (unless (monsters-dead)
		    (monster-hit (random-monster) 1))))))

;;; helper function

(defun randval (n )
  (1+ (random (max 1 n))))

;;pick a random monster out of the array
(defun random-monster ()
  (let (( m (aref *monsters* (random (length *monster*)))))
    (if (monster-dead m)
	(random-monster)
	m)))

(defun pick-monster ()
  (fresh-line)
  (princ "Monster #: ")
  (let (( x (read)))
    (if (not (and (integerp x) (>= x 1) (<=  x *monster-num* )))
	(progn (princ "That is not a valid monster number.")
	       (pick-monster))
	(let (( m (aref *monster* (1- x))))
	  (if (monster-dead m )
	      (progn (princ "That monster is already dead.")
		     (pick-monster))
	      m)))))


;;; monster manger function

(defun init-monsters ()
  (setf *monsters* 
	(map 'vector
	     (lambda (x)
	       (funcall (nth (random (length *monster-builders*))
			     *monster-builders*)))
	     (make-array *monster-num*))))

