(defun fib (n)
  (if (< n 2)
      1
      (+ (fib (- n 1) ) (fib (- n 2)))))

;;测试time 尾部递归执行时间太差了
(time ( fib 40))
