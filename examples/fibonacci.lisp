(defun fibonacci (n)
  (if (> 2 n)
      n
      (+ (fibonacci (- n 1))
         (fibonacci (- n 2)))))
(fibonacci 20)