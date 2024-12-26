
;;; 1
(defun my-reverse-inner (lst tmp)
  (cond ((not lst) tmp)
        ((my-reverse-inner (cdr lst) (cons (car lst) tmp)))))

(defun my-reverse (lst)
  (my-reverse-inner lst nil))

;;; 2
(defun my-listp (lst)
  (cond ((not lst) t)
        ((atom lst) nil)
        ((my-listp (cdr lst)))))

(defun get-first-list (lst)
  (cond ((not lst) nil)
        ((and (my-listp (car lst)) (/= 0 (length (car lst)))) (car lst))
        ((get-first-list (cdr lst)))))

;;; 3
(defun mul-list-inner (lst val tmp)
  (cond ((not lst) tmp)
        ((numberp (car lst))
         (mul-list-inner (cdr lst)
                         val
                         (append tmp
                                 (cons (* val (car lst))
                                       nil))))
        ((mul-list-inner (cdr lst)
                         val
                         (append tmp
                                 (cons (car lst)
                                       nil))))))

(defun mul-list (lst val) (mul-list-inner lst val nil))

;;; 4
(defun select-between-inner (lst min max tmp)
  (cond ((not lst) tmp)
        ((< min (car lst) max) (select-between-inner (cdr lst) min max (cons (car lst) tmp)))
        ((select-between-inner (cdr lst) min max tmp))))

(defun select-between (lst min max)
  (select-between-inner lst min max nil))

;;; 5
(defun rec-add-1-inner (lst sum)
  (cond ((not lst) sum)
        ((numberp (car lst)) (rec-add-1-inner (cdr lst) (+ sum (car lst))))
        ((rec-add-1-inner (cdr lst) sum))))

(defun rec-add-1 (lst)
  (rec-add-1-inner lst 0))

(defun rec-add-2-inner (lst sum)
  (cond ((not lst) sum)
        ((numberp (car lst)) (rec-add-2-inner (cdr lst) (+ sum (car lst))))
        ((my-listp (car lst)) (rec-add-2-inner (cdr lst) (+ sum (rec-add-2-inner (car lst) 0))))
        ((rec-add-2-inner (cdr lst) sum))))

(defun rec-add-2 (lst)
  (rec-add-2-inner lst 0))

