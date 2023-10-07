(defun fibonacci (n)
  (if (> 2 n)
      n
      (+ (fibonacci (- n 1)) ; comment
         (fibonacci (- n 2)))))
(fibonacci 20)
