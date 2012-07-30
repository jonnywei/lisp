;;;;wjj test
;;;;wjj test

(defun wjjtest()
  "this is wjj first lisp test function!"
  (+ 3 4)
  (format t "test2"))

(defun verbose-sum ( x y )
  (format t  "~a+~a=~a" x y 
  (+ x y)))

(defun optionalparam ( x &optional y z)
  (list x y z))

(defun optionalparam-default ( x &optional (y 10 ) z)
  (list x y z))

(defun optionalparam-default-supplyed ( x &optional (y 20 y-supplied-p) z)
  (list x y z y-supplied-p ))

(defun rest-param ( &rest  x )
  (list x ))

(defun key-param ( &key  a b c)
  (list a b c))

(defun key-param-default-supplied ( &key (first 30 ) (second 20 second-supplied-p))
  (list first second second-supplied-p))

(defun param-optional-rest-key ( x &optional y  &rest z &key m n )
  (list x y z m  n ))


;;;;总结，key关键字形参比较特殊，尽量不要与optional或者rest组合在一起。否则可能出问题