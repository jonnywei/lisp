;;;; dice of doom v1
;;;; simple

;;;first define some global variables

(defvar *num-players* 2)
(defvar *max-dice* 3)
(defvar *board-size* 2)
(defvar *board-hexnum* (* *board-size* *board-size*))

;;; define board array , not list for performace reson
;;; the item of array  is a list, the first of item is player number, and second is dice number
;;; e.g  ((0 3) (0 1) (1 2 ) (1 3))  means the board has four hexagon and player 0 has 3 dice in place 1 adn 1 dice in place 2

(defun board-array(lst)
  (make-array *board-hexnum* :initial-contents lst))

;; when game begin , we'll create randomized board

(defun gen-board()
  (board-array( loop for n below *board-hexnum*
		     collect ( list (random *num-players*)
				    (1+ (random *max-dice*))))))

;;we user letters name player 
(defun player-letter(n)
  (code-char (+ 97 n)))

;;draw board on the console

(defun draw-board(board)
  (loop for y below *board-size*
       do (progn (fresh-line)
		 (loop repeat (- *board-size* y)
		    do (princ "  "))
		 (loop for x below *board-size* 
		      for hex = (aref  board (+ x (* *board-size* y)))
		      do (format t "~a-~a " (player-letter (first hex))
				 (second hex))))))

;;; generating a game tree
;;; format like (player board (moves))
;;;

;;game tree

(defun game-tree( board player spare-dice first-move)
  (list player
	board
	(add-passing-move board
			  player
			  spare-dice
			  first-move
			  (attacking-moves board player spare-dice))))

;;calculating the passing move
;;如果是第一次移动的话，返回原moves 如果不是则添加nil表示无法移动
(defun add-passing-move ( board player spare-dice first-move moves)
  (if first-move
      moves
      (cons (list nil
		  (game-tree ( add-new-dice board player (1- spare-dice))
			     (mod (1+ player) *num-players* )
			     0
			     t))
	    moves)))

;;calculating attacking moves
(defun attacking-moves (board cur-player spare-dice)
  (labels ((player (pos)
	     (car (aref board pos)))
	   (dice (pos)
	     (cadr (aref board pos))))
    (mapcan (lambda (src)
	      (when (eq (player src) cur-player)
		(mapcan (lambda (dst)
			  (when ( and ( not (eq (player dst) cur-player)) (> (dice src) (dice dst)))
			    (list (list (list src dst)
					(game-tree (board-attack board cur-player src dst (dice src))
						   cur-player
						   (+ spare-dice (dice dst))
						   nil)))))
			(neighbors src))))
	    (loop for n below *board-hexnum*  
		 collect n))))

;;find neighbors
;; clean functional

(defun neighbors (pos)
  (let (( up (- pos *board-size*))
	(down (+ pos *board-size*)))
    (loop for p in (append (list up down)
			   (unless (zerop (mod pos *board-size*))
			     (list (1- up) (1- pos)))
			   (unless (zerop (mod (1+ pos) *board-size*))
			     (list (1+ pos) (1+ down ))))
	 when (and (>= p 0) (< p *board-hexnum* ))
	 collect p )))
    
;; attacking 
;; clean functional function
(defun board-attack ( board player src dist dice)
  (board-array (loop for pos
		  for hex across board
		      collect (cond ((eq pos src ) (list player 1))
				    ((eq pos dst ) (list player (1- dice)))
				    (t hex))))))

