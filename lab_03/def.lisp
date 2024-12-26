
(defun imag-sqrt (val)
  (if (<= 0 val) (list (sqrt val) 0) (list 0 (sqrt (- val)))))

(defun solve-sqr (a b c)
  (let ((disc (- (* b b) (* (* 4 a) c))) (a2 (* 2 a)))
    (cond ((> 1e-8 (abs disc)) (list (list (/ (- b) a2) 0)))
          ((let ((base (list (/ (- b) a2) 0)) (sdisc (imag-sqrt (/ disc (* a2 a2)))))
             (list (mapcar #'- base sdisc) (mapcar #'+ base sdisc))))
          )))

