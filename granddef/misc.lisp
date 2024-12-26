(load "arith.lisp")

(defun linspace-inner (current step n tmp arith)
  (if (= 0 n)
      tmp
      (linspace-inner (Arith-add arith current step)
                      step
                      (Arith-sub arith n 1)
                      (append tmp (cons current nil))
                      arith)))

(defun linspace (start end amount arith)
  (linspace-inner start
                  (Arith-div arith
                             (Arith-sub arith end start)
                             (Arith-sub arith amount 1))
                  amount nil arith))

(defun int2 (a b frac arith) (Arith-add arith
                                        (Arith-mul arith
                                                   (Arith-sub arith 1 frac)
                                                   a)
                                        (Arith-mul arith frac b)))
(defun mid (a b arith) (int2 a b 0.5 arith))

(defun table-func-inner (x x-list y-list arith)
  (cond ((not (cadr x-list)) (car y-list))
        ((< (cadr x-list) x) (table-func-inner x (cdr x-list) (cdr y-list) arith))
        ((int2 (car y-list)
               (cadr y-list)
               (Arith-div arith
                          (Arith-sub arith x (car x-list))
                          (Arith-sub arith (cadr x-list) (car x-list)))
               arith))))

(defun table-func (x x-list y-list arith)
  (if (> (car x-list) x) (car y-list) (table-func-inner x x-list y-list arith)))

(defun get-table-func (x-list y-list)
  (let ((x-c (copy-list x-list))
        (y-c (copy-list y-list)))
    (lambda (x arith) (table-func x x-c y-c arith))))

(defun func-trans (func scale start end)
  (lambda (x arith)
    (let ((x-s (Arith-add arith
                          (Arith-mod arith
                                     (Arith-sub arith x start)
                                     (Arith-sub arith end start))
                          start)))
      (Arith-mul arith (funcall func x-s arith) scale))))

