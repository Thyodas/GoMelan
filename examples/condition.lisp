(defun even? (x)
    (= (mod x 2) 0))

(defun check-and-multiply (x y)
    (if (and (even? x) (even? y))
            (* x y)
            -1))

(check-and-multiply 2 4) ; Returns 8
(check-and-multiply 2 3) ; Returns -1
