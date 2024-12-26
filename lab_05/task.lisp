
(defun subtrcact10 (lst)
  (mapcar (lambda (x) (if (numberp x) (- x 10) x)) lst))

(defun sqr-list (lst) (mapcar '* lst lst))

(defun mul-list (lst val)
  (mapcar (lambda (x) (if (numberp x) (* x val) x)) lst))

(defun palindromep (lst) (apply 'and (mapcar 'equal lst (reverse lst))))

(defun fand (a b) (if (and a b) T))

(defun set-equal (set1 set2)
  (cond ((/= (length set1) (length set2)) nil)
        ((reduce 'fand (mapcar (lambda (x) (member x set1 :TEST 'equal)) set2)))))

(defun select-between (lst min max)
  (mapcan (lambda (x) (if (< min x max) (cons x nil) nil)) lst))

(defun times (a b) (mapcan (lambda (x) (mapcar (lambda (y) (list x y)) b)) a))

(defun sum-length (lst) (apply '+ (mapcar 'length lst)))

