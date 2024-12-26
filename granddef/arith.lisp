
(defstruct Arith _add _sub _mul _div)

(define-condition NOT-ARITH (error)
  ((message :initform nil :reader message)))

(defun Arith-add (arith &rest arg) (apply (Arith-_add arith) arg))
(defun Arith-sub (arith &rest arg) (apply (Arith-_sub arith) arg))
(defun Arith-mul (arith &rest arg) (apply (Arith-_mul arith) arg))
(defun Arith-div (arith &rest arg) (apply (Arith-_div arith) arg))

(defun Arith-mod (arith x y) (Arith-sub arith
                                        x
                                        (Arith-mul arith
                                                   y
                                                   (floor (Arith-div arith
                                                                     x
                                                                     y)))))

(defun pow (arith x y)
  (if (= 1 y)
      x
      (let* ((half (pow arith x (floor (Arith-div arith y 2))))
             (tmp (Arith-mul arith half half)))
        (if (= 0 (Arith-mod arith y 2)) tmp (Arith-mul arith tmp x)))))

(defun Arith-pow (arith x y) (cond ((= 0 y) 1)
                                   ((and (integerp y))
                                    (if (< 0 y)
                                        (pow arith x y)
                                        (pow arith (Arith-div arith 1 x) (Arith-sub arith y))))
                                   (0)))

(defun exp-inner (xacc facc x n res arith)
  (let* ((add (Arith-div arith xacc facc))
         (new (Arith-add arith res add)))
    (if (< 1e-8 (abs add))
        (exp-inner (Arith-mul arith xacc x)
                   (Arith-mul arith facc n)
                   x (Arith-add arith n 1) new arith)
        new)))


(defun Arith-exp (arith x) (exp-inner x 1 x 2 1 arith))

