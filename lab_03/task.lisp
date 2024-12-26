
(defun g-even (num)
  (cond ((oddp num) (+ num 1))
        (T num)))

(defun g-even (num) (+ num (mod num 2)))


(defun mod-sum (num)
  (cond ((> num 0) (+ num 1))
        ((< num 0) (- num 1))
        (T 0)))

(defun mod-sum (num)
  (+ num (/ num (abs num))))

(defun mod-sum (num)
  (cond ()
        ()))


(defun sort-2 (num1 num2)
  (cond ((> num1 num2) (list num2 num1))
        (T (list num1 num2))))


(defun between (num num-l num-h)
  (or (and (< num-l num) (< num num-h))
      (and (> num-l num) (> num num-h))))

(defun between (num num-l num-h)
  (or (< num-l num num-h)
      (> num-l num num-h)))


(defun ge (num1 num2)
  (let ((dif (- num2 num1))) (zerop (+ dif (abs dif)))))
  ; (zerop (+ (- num2 num1) (abs (- num2 num1)))))
  ; (>= num1 num2))


(defun between-if (num num-l num-h)
  (if (< num-l num)
      (< num num-h)
      (if (/= num-l num) (> num num-h) nil)))

(defun between-cond (num num-l num-h)
  (cond ((< num-l num) (< num num-h))
        ((> num-l num) (> num num-h))
        (T nil)))

(defun between-cond (num num-l num-h)
  (cond ((< num-l num num-h))
        ((> num-l num num-h))))

(defun between-and-or (num num-l num-h)
  (or (and (< num-l num) (< num num-h))
      (and (> num-l num) (> num num-h))))


(defun how-alike (x y)
  (cond ((or (= x y) (equal x y)) 'the_same)
        ((and (oddp x) (oddp y)) 'both_odd)
        ((and (evenp x) (evenp y)) 'both_even)
        (T 'difference)))

(defun how-alike-if (x y)
  (if (or (= x y) (equal x y))
      'the_same
      (if (and (oddp x) (oddp y))
          'both_odd
          (if (and (evenp x) (evenp y))
              'both_even
              'difference))))

(defun how-alike-if (x y)
  (if (/= x y)
      (if (not (equal x y))
          (if (oddp x)
              (if (oddp y) 'both_odd 'difference)
              (if (evenp y) 'both_even 'difference))
          'both_same)
      'both_same))

(defun how-alike-and-or (x y)
  (or (and (or (= x y) (equal x y)) 'the_same)
      (and (and (oddp x) (oddp y)) 'both_odd)
      (and (and (evenp x) (evenp y)) 'both_even)
      'difference))

