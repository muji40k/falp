(ql:quickload "ieee-floats")
(load "arith.lisp")

(defun not-bin (x) (if (= 0 x) 1 0))
(defun add-bin (x y)
  (cond ((and (= x 0) (= y 0)) '(0 0))
        ((and (= x 0) (= y 1)) '(1 0))
        ((and (= x 1) (= y 0)) '(1 0))
        ((and (= x 1) (= y 1)) '(0 1))))

(defun sub-bin (x y &key borrow)
  (if (or (not borrow) (= borrow 0))
      (cond ((and (= x 0) (= y 0)) '(0 0))
            ((and (= x 0) (= y 1)) '(1 1))
            ((and (= x 1) (= y 0)) '(1 0))
            ((and (= x 1) (= y 1)) '(0 0)))
      (cond ((and (= x 0) (= y 0)) '(1 1))
            ((and (= x 0) (= y 1)) '(0 1))
            ((and (= x 1) (= y 0)) '(0 0))
            ((and (= x 1) (= y 1)) '(1 1)))))

(defun not-list (lst) (mapcar 'not-bin lst))

(defun neg-list (lst) (cons (not-bin (car lst)) (copy-list (cdr lst))))

(defun abs-list (lst) (cons 0 (cdr lst)))

(defun add-list-inner (lst1 lst2 shift res)
  (cond ((and (not lst1) (not lst2)) (if (= 0 shift)
                                         res
                                         (append res (list 1))))
        ((not lst1) (if (= 1 shift)
                        (add-list-inner '(1) lst2 0 res)
                        (append res (copy-list lst2))))
        ((not lst2) (if (= 1 shift)
                        (add-list-inner lst1 '(1) 0 res)
                        (append res (copy-list lst1))))
        ((let* ((csum (add-bin (car lst1) (car lst2)))
                (ssum (add-bin (car csum) shift))
                (cshift (add-bin (cadr csum) (cadr ssum))))
           (add-list-inner (cdr lst1) (cdr lst2) (car cshift) (append res (cons (car ssum) nil)))))))

(defun add-list (lst1 lst2)
  (cond ((= (car lst1) (car lst2) 0) (cons 0 (add-list-inner (cdr lst1) (cdr lst2) 0 nil)))
        ((= (car lst1) (car lst2) 1) (neg-list (add-list (neg-list lst1) (neg-list lst2))))
        ((= (car lst1) 0) (sub-list lst1 (neg-list lst2)))
        ((= (car lst2) 0) (sub-list lst2 (neg-list lst1)))))

(defun normalise-list-inner (lst res current)
  (cond ((not lst) res)
        ((= 1 (car lst)) (normalise-list-inner (cdr lst) (append res current (list 1)) nil))
        ((normalise-list-inner (cdr lst) res (append current (list 0))))))

(defun normalise-list (lst) (normalise-list-inner lst nil nil))

(defun normalise-list-sign (lst) (cons (car lst) (normalise-list (cdr lst))))

(defun correct-sub (res borrow)
  (if (= 0 borrow)
      (cons 0 (normalise-list res))
      (cons 1 (normalise-list (add-list-inner (not-list res) '(1) 0 nil)))))

(defun sub-list-inner (lst1 lst2 borrow res)
  (cond ((and (not lst1) (not lst2)) (correct-sub res borrow))
        ((not lst1) (sub-list-inner '(0) lst2 borrow res))
        ((not lst2) (sub-list-inner lst1 '(0) borrow res))
        ((let* ((s (sub-bin (car lst1) (car lst2) :borrow borrow)))
           (sub-list-inner (cdr lst1) (cdr lst2) (cadr s) (append res (cons (car s) nil)))))))

(defun sub-list (lst1 lst2)
  (cond ((= (car lst1) (car lst2) 0) (sub-list-inner (cdr lst1) (cdr lst2) 0 nil))
        ((= (car lst1) (car lst2) 1) (sub-list (neg-list lst2) (neg-list lst1)))
        ((= (car lst1) 0) (add-list lst1 (neg-list lst2)))
        ((= (car lst2) 0) (neg-list (add-list (neg-list lst1) lst2)))))

(defun mul-list-inner (lst1 lst2 res current)
  (cond ((not lst2) (append res current))
        ((= 0 (car lst2)) (mul-list-inner lst1 (cdr lst2) (append res (cons (if (not current) 0 (car current)) nil)) (cdr current)))
        ((let ((newcurrent (add-list-inner current lst1 0 nil)))
           (mul-list-inner lst1 (cdr lst2) (append res (cons (car newcurrent) nil)) (cdr newcurrent))))))

(defun mul-list (lst1 lst2)
  (append (cons (car (sub-bin (car lst1) (car lst2))) nil) (mul-list-inner (cdr lst1) (cdr lst2) nil nil)))

(defun truncate-list-rest-copy-rev (lst target res) 
  (if (eq lst target)
      res
      (truncate-list-rest-copy-rev (cdr lst) target (cons (car lst) res))))

(defun truncate-list-rest-inner (stack target)
  (if (not (cadr stack))
      (truncate-list-rest-copy-rev (car stack) target nil)
      (truncate-list-rest-inner (cdr stack) target)))

(defun truncate-list-rest (stack)
  (truncate-list-rest-inner stack (car stack)))

(defun truncate-list-back (stack1 stack2)
  (cond ((not (cadr stack2)) (list (car stack1) (truncate-list-rest stack1)))
        ((truncate-list-back (cdr stack1) (cdr stack2)))))

(defun truncate-list-align-start (lst1 lst2 stack1 stack2)
  (cond ((and (not lst1) (not lst2)) (truncate-list-back stack1 stack2))
        ((not lst1) '(nil nil))
        ((not lst2) (truncate-list-align-start (cdr lst1) nil (cons lst1 stack1) stack2))
        ((truncate-list-align-start (cdr lst1) (cdr lst2) (cons lst1 stack1) stack2))))

(defun truncate-list (lst1 lst2) (truncate-list-align-start lst1 lst2 nil lst2))

(defun get-rest (rest) (if (not rest) 0 (car rest)))
(defun reverse-rest (rest res) (if (not rest) res (reverse-rest (cdr rest) (cons (car rest) res))))

(defun div-list-main (lst1 lst2 rest res offset stop body)
  (cond ((not stop) (list res (sub-list offset '(0 1))))
        ((and (not rest) (not (cadr lst1)) (= 0 (car lst1)))
         (list res (sub-list offset '(0 1))))
        ((let ((tmp (sub-list (cons 0 lst1) (cons 0 lst2))))
           (if (= 1 (car tmp))
               (div-list-main (cons (get-rest rest) lst1)
                              lst2
                              (cdr rest)
                              (if (not body) res (cons 0 res))
                              (if (not rest) (add-list offset '(0 1)) offset)
                              (if (not body) stop (cdr stop))
                              body)
               (if (and (not (cdr tmp)) (not (normalise-list rest)))
                   (list (reverse-rest rest (cons 1 res)) offset)
                   (div-list-main (cons (get-rest rest) (cdr tmp))
                                  lst2
                                  (cdr rest)
                                  (cons 1 res)
                                  (if (not rest) (add-list offset '(0 1)) offset)
                                  (cdr stop)
                                  t)))))))

(defun div-list-inner (lst1 lst2 stop)
  (let ((c (truncate-list lst1 lst2)))
    (if (not (car c))
        (div-list-main lst1 lst2 nil nil '(0) stop nil)
        (div-list-main (car c) lst2 (cadr c) nil '(0) stop nil))))

(defun div-list (lst1 lst2 &key (max-digits 64))
  (if (not (cdr lst1))
      '((0) (0))
      (let ((res (div-list-inner (cdr lst1) (cdr lst2) (make-list max-digits :initial-element 1))))
        (list (cons (car (add-bin (car lst1) (car lst2))) (car res))
              (cadr res)))))

(defun shift-list-inner (lst off sgn)
  (if (and (not (null lst)) (= 0 (car lst)))
      (shift-list-inner (cdr lst) (add-list off '(0 1)) sgn)
      (list (cons sgn lst) off)))

(defun shift-list (lst) (shift-list-inner (cdr lst) '(0) (car lst)))

(defstruct ListNum exp mantissa)

(defun parse-bits-inner (num amount res)
  (if (not (cadr amount))
      res
      (parse-bits-inner (ash num -1) (sub-list amount '(0 1)) (append res (cons (logand num 1) nil)))))

(defun parse-bits (num amount) (parse-bits-inner num amount nil))

(defun int-to-list (num)
  (let ((main (parse-bits num '(0 0 0 0 0 0 1)))
        (sign (logand (ash num -31) 1)))
    (if (= 0 sign)
        (make-ListNum :exp '(0)
                      :mantissa (cons 0 (normalise-list main)))
        (make-ListNum :exp '(0)
                      :mantissa (correct-sub main 1)))))

(defun float-to-list (num)
  (let* ((num (ieee-floats:encode-float64 num))
         (mantissa-raw (append (parse-bits num '(0 0 0 1 0 1 1)) (list 1)))
         (exp-raw (cons 0 (parse-bits (ash num -52) '(0 1 1 0 1))))
         (sign (logand (ash num -63) 1))
         (shift (shift-list (cons sign mantissa-raw))))
    (make-ListNum :exp (add-list (add-list '(1 0 0 1 0 1 1) (cadr shift)) (sub-list exp-raw '(0 1 1 1 1 1 1 1 1 1 1)))
                  :mantissa (car shift))))

(defun rational-to-list (num) (float-to-list (float num)))

(defun conv-to-list (num &key (rules '((integerp int-to-list)
                                       (floatp float-to-list)
                                       (rationalp rational-to-list))))
  (if (not rules)
      nil
      (let ((rule (car rules)))
        (if (funcall (car rule) num)
            (funcall (cadr rule) num)
            (conv-to-list num :rules (cdr rules))))))

(defun lst-length-inner (lst size)
  (if (not lst)
      size
      (lst-length-inner (cdr lst) (add-list size '(0 1)))))

(defun lst-length (lst) (lst-length-inner lst '(0)))

(defun list-to-bits-inner (lst cnt res)
  (cond ((not lst) res)
        ((= 1 (car lst)) (list-to-bits-inner (cdr lst) (ash cnt 1) (logxor res cnt)))
        ((list-to-bits-inner (cdr lst) (ash cnt 1) res))))

(defun list-to-bits (lst) (list-to-bits-inner lst 1 0))

(defun list-to-double-mantissa (num res) nil)

(defun list-to-double-exp-inner (exp res) nil)

(defun expand-raw (lst num)
  (if (not (cdr num))
      lst
      (expand-raw (cons 0 lst) (sub-list num '(0 1)))))

(defun list-to-double-exp (num res)
  (let* ((tr-res (car (truncate-list (cdr (ListNum-mantissa num)) (make-list 53 :initial-element 1))))
         (tr (if (not tr-res) (cdr (ListNum-mantissa num)) tr-res))
         (off (sub-list '(0 1 0 1 0 1 1) (lst-length tr)))
         (exp-off (sub-list (lst-length (cdr (ListNum-mantissa num))) '(0 1)))
         (man (expand-raw tr off))
         (exp-c (add-list (add-list (ListNum-exp num) exp-off) '(0 1 1 1 1 1 1 1 1 1 1))))
    (logxor (ash (logxor (ash res 11) (list-to-bits (cdr exp-c))) 52) (logand (list-to-bits man) 4503599627370495))))

(defun list-to-double-sign (num res)
  (if (not (cdr (ListNum-mantissa num)))
      0
      (if (= 1 (car (ListNum-mantissa num)))
          (list-to-double-exp num 1)
          (list-to-double-exp num 0))))

(defun list-to-double (num)
  (ieee-floats:decode-float64 (list-to-double-sign num 0)))

(defun expand-loop (num rest)
  (if (not (cdr rest))
      num
      (expand-loop (cons 0 num) (sub-list rest '(0 1)))))

(defun expand-inner (num exp diff)
  (let ((lnum (ListNum-mantissa num)))
    (make-ListNum :exp exp
                  :mantissa (cons (car lnum) (expand-loop (cdr lnum) diff)))))

(defun expand (num1 num2)
  (let ((diff (sub-list (ListNum-exp num1) (ListNum-exp num2))))
    (cond ((not (cdr diff)) (list (ListNum-exp num1) num1 num2))
          ((= 1 (car diff)) (list (ListNum-exp num1) num1 (expand-inner num2 (ListNum-exp num1) (abs-list diff))))
          ((list (ListNum-exp num2) (expand-inner num1 (ListNum-exp num2) (abs-list diff)) num2)))))

(defun my-add2 (a b)
  (let ((res (expand a b)))
    (make-ListNum :exp (car res)
                  :mantissa (normalise-list-sign (apply (lambda (x y) (add-list (ListNum-mantissa x) (ListNum-mantissa y))) (cdr res))))))

(defun my-sub2 (a b)
  (let ((res (expand a b)))
    (make-ListNum :exp (car res)
                  :mantissa (normalise-list-sign (apply (lambda (x y) (sub-list (ListNum-mantissa x) (ListNum-mantissa y))) (cdr res))))))

(defun my-mul2 (a b)
  (make-ListNum :exp (add-list (ListNum-exp a) (ListNum-exp b))
                :mantissa (normalise-list-sign (mul-list (ListNum-mantissa a) (ListNum-mantissa b)))))

(defun my-div2 (a b)
  (let ((tmp (div-list (ListNum-mantissa a) (ListNum-mantissa b))))
    (make-ListNum :exp (sub-list (sub-list (ListNum-exp a) (ListNum-exp b)) (cadr tmp))
                  :mantissa (normalise-list-sign (car tmp)))))

(defun _my-add (rest) (reduce 'my-add2 (mapcar 'conv-to-list rest) :initial-value (make-ListNum :exp '(0) :mantissa '(0))))
(defun _my-sub (rest)
  (cond ((not rest) (make-ListNum :exp '(0) :mantissa '(0)))
        ((not (cadr rest)) (my-sub2 (make-ListNum :exp '(0) :mantissa '(0)) (conv-to-list (car rest))))
        ((my-sub2 (conv-to-list (car rest)) (reduce 'my-add2 (mapcar 'conv-to-list (cdr rest)) :initial-value (make-ListNum :exp '(0) :mantissa '(0)))))))
(defun _my-mul (rest) (reduce 'my-mul2 (mapcar 'conv-to-list rest) :initial-value (make-ListNum :exp '(0) :mantissa '(0 1))))
(defun _my-div (rest)
  (cond ((not rest) (make-ListNum :exp '(0) :mantissa '(0 1)))
        ((not (cadr rest)) (my-div2 (make-ListNum :exp '(0) :mantissa '(0 1)) (conv-to-list (car rest))))
        ((my-div2 (conv-to-list (car rest)) (reduce 'my-mul2 (mapcar 'conv-to-list (cdr rest)) :initial-value (make-ListNum :exp '(0) :mantissa '(0 1)))))))

(defun my-add (&rest rest) (list-to-double (_my-add rest)))
(defun my-sub (&rest rest) (list-to-double (_my-sub rest)))
(defun my-mul (&rest rest) (list-to-double (_my-mul rest)))
(defun my-div (&rest rest) (list-to-double (_my-div rest)))

(defun make-list-arith ()
  (make-Arith :_add 'my-add :_sub 'my-sub :_mul 'my-mul :_div 'my-div))

