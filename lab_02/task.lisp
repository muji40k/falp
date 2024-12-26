
(defun hypotenuse (a b) (sqrt (+ (* a a) (* b b))))

(defun longer-then (a b)
  (cond ((and (listp a) (listp b)) (> (length a) (length b)))
        (T nil)))

(defun longer-then (a b) (> (length a) (length b)))

(defun f-to-c (temp) (* (/ 5.0 9.0) (- temp 32)))

