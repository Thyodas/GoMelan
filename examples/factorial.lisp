(defun add
    (a b)
    (+ a b))
(add 3 4)
(defun factorial (n)
    (if (> 1 n)
            1
            (* n (factorial (- n 1)))))
(factorial 20)