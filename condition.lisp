;;;; 
;;;; lisp condition excerise
;;;;

(defun divide (n d )
  (when (zerop d)
    (error "Sorry, you can't divide by zero."))
  (/ n d))



;;;defun a condition
(define-condition  whats-wrong (error )
  ((what :initarg :what
	 :initform "somthing"
	 :reader what))
  (:report (lambda (condition stream)
	     (format stream  "Foo! ~@(~A~) is wrong"
		     (what condition))))
  (:documentation "Tell the user that something is wrong." ))

(define-condition whats-wrong-and-why (whats-wrong)
  ((why :initarg :why
	:initform  "no clude"
	:reader why))
  (:report (lambda (condition stream)
	     (format stream "~@(~A~) is wrong.Why? ~@(~A~). "
		     (what condition)
		     (why condition)))))


(define-condition high-disk-utilization ()
  ((disk-name :initarg :disk-name
	       :reader disk-name)
   (current :initarg :current
	    :reader current-utilization)
   (threshold :initarg :threshold
	      :reader threshold))
(:report (lambda ( condition stream )
	   (format stream "Disk ~A is ~D full; theshold is ~D%."
		   (disk-name condition)
		   (current-utilization condition)
		   (threshold condition)))))
		   
