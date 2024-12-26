;;; 2. Используя только функции CAR и CDR, написать выражения, возвращающие
;;; заданный элемент списка.

;;; 1) второй
(car (cdr '(a b c d)))

;;; 2) третий
(car (cdr (cdr '(a b c d))))

;;; 3) четвертый
(car (cdr (cdr (cdr '(a b c d)))))


;;; 3. Что будет в результате вычисления выражений?
(CAADR '((blue cube) (red pyramid)))
;;; red

(CADR '((abc) (def) (ghi)))
;;; (def)

(CDAR '((abc) (def) (ghi)))
;;; Nil

(CADDR '((abc) (def) (ghi)))
;;; (ghi)


;;; 4. Напишите результат вычисления выражений и объясните как он получен:

(list 'Fred 'and 'Wilma)
;;; (FRED AND WILMA)

(list 'Fred '(and Wilma))
;;; (FRED (AND WILMA))

(cons Nil Nil)
;;; (Nil)

(cons T Nil)
;;; (T)

(cons Nil T)
;;; (NIL . T)

(list Nil)
;;; (NIL)

(cons '(T) Nil)
;;; ((T))

(list '(one two) '(free temp))
;;; ((ONE TWO) (FREE TEMP))

(cons 'Fred '(and Wilma))
;;; (FRED AND WILMA)

(cons 'Fred '(Wilma))
;;; (FRED WILMA)

(list Nil Nil)
;;; (NIL NIL)

(list T Nil)
;;; (T NIL)

(list Nil T)
;;; (NIL T)

(cons T (list Nil))
;;; (T NIL)

(list '(T) Nil)
;;; ((T) NIL)

(cons '(one two) '(free temp))
;;; ((ONE TWO) FREE TEMP)


;;; 5. Написать лямбда-выражение и соответствующую функцию:

;;; - Написать функцию (f arl ar2 ar3 ar4), возвращающую список: ((arl ar2) (ar3 ar4)).
((lambda (arl ar2 ar3 ar4)
   (cons (cons arl (cons ar2 nil)) (cons (cons ar3 (cons ar4 nil)) nil)))
 1 2 3 4)

(defun f1-cons (arl ar2 ar3 ar4)
  (cons (cons arl (cons ar2 nil)) (cons (cons ar3 (cons ar4 nil)) nil)))

((lambda (arl ar2 ar3 ar4) (list (list arl ar2) (list ar3 ar4))) 1 2 3 4)

(defun f1-list (arl ar2 ar3 ar4)
  (list (list arl ar2) (list ar3 ar4)))

;;; - Написать функцию (f arl ar2), возвращающую ((arl) (ar2)).
((lambda (arl ar2)
  (cons (cons arl nil) (cons (cons ar2 nil) nil))) 1 2)

(defun f2-cons (arl ar2)
  (cons (cons arl nil) (cons (cons ar2 nil) nil)))

((lambda (arl ar2)
   (list (list arl) (list ar2))) 1 2)

(defun f2-list (arl ar2)
  (list (list arl) (list ar2)))

;;; - Написать функцию (f arl), возвращающую (((arl))).
((lambda (arl) (cons (cons (cons arl nil) nil) nil)) 1)

(defun f3-cons (arl) (cons (cons (cons arl nil) nil) nil))

((lambda (arl) (list (list (list arl)))) 1)

(defun f3-list (arl) (list (list (list arl))))

