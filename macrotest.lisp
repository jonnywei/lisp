;;;; macro test


(defmacro let1 (var val &body body)
  `(let ( ( ,var ,val))
     ,@body))


;;; example 2 

(defun my-length (lst )
  (labels ( (f (lst acc)
	      (if lst
		  (f (cdr lst) (1+ acc))
		  acc)))
    (f lst 0)))


;;; example split 

;; Warning Contains Bugs

(defmacro split ( val yes no)
  `(if ,val
       (let ((head (car ,val))
	     (tail (cdr ,val)))
	 ,yes)
       ,no))


(defun my-length2 (lst)
  (labels ((f (lst acc)
	     (split lst
		    (f tail (1+ acc))
		    acc)))
    (f lst 0)))

	     