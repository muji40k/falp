(load "arith.lisp")

(defstruct Matrix3D size A B C D)
(defstruct InitialValue m-val k-val p-val)

(defun Matrix3D-check (matrix)
  (= (Matrix3D-size matrix)
     (length (Matrix3D-A matrix))
     (length (Matrix3D-B matrix))
     (length (Matrix3D-C matrix))
     (length (Matrix3D-D matrix))))

(define-condition NOT-MATRIX3D (error)
  ((message :initform nil :reader message)))

(define-condition INVALID-MATRIX3D (error)
  ((message :initform nil :reader message)))

(define-condition NOT-INITIALVALUE (error)
  ((message :initform nil :reader message)))

;;; (defun runthrough-step (pk pb a b c d end arith)
;;;   (if (not (car a))
;;;       (cons (Arith-div arith
;;;                        (Arith-sub arith
;;;                                   (InitialValue-p-val end)
;;;                                   (Arith-mul arith
;;;                                              (InitialValue-m-val end)
;;;                                              pb))
;;;                        (Arith-add arith
;;;                                   (InitialValue-k-val end)
;;;                                   (Arith-mul arith (InitialValue-m-val end)
;;;                                              pk)))
;;;             nil)
;;;       (let* ((ck (Arith-div arith
;;;                             (car c)
;;;                             (Arith-sub arith
;;;                                        (car b)
;;;                                        (Arith-mul arith
;;;                                                   (car a)
;;;                                                   pk))))
;;;              (cb (Arith-div arith
;;;                             (Arith-add arith
;;;                                        (car d)
;;;                                        (Arith-mul arith (car a)
;;;                                                   pb))
;;;                             (Arith-sub arith
;;;                                        (car b)
;;;                                        (Arith-mul arith
;;;                                                   (car a)
;;;                                                   pk))))
;;;              (r (runthrough-step ck cb (cdr a) (cdr b) (cdr c) (cdr d) end arith)))
;;;         (cons (Arith-add arith cb (Arith-mul arith ck (car r))) r))))
;;;  
;;; (defun runthrough-inner (matrix start end arith)
;;;   (let* ((ck (Arith-div arith
;;;                         (Arith-sub arith (InitialValue-k-val start))
;;;                         (InitialValue-m-val start)))
;;;          (cb (Arith-div arith
;;;                         (InitialValue-p-val start)
;;;                         (InitialValue-m-val start)))
;;;          (r (runthrough-step ck cb (Matrix3D-A matrix) (Matrix3D-B matrix)
;;;                              (Matrix3D-C matrix) (Matrix3D-D matrix) end arith)))
;;;     (cons (Arith-add arith cb (Arith-mul arith ck (car r))) r)))

(defun runthrough-back-run (k-stack res arith)
  (if (not (null k-stack))
      (let ((k (car k-stack)))
        (runthrough-back-run (cdr k-stack)
                             (cons (Arith-add arith
                                              (cadr k)
                                              (Arith-mul arith (car k) (car res)))
                                   res)
                             arith))
      res))

(defun runthrough-k-run (a b c d end k-stack arith)
  (let ((k (car k-stack)))
    (if (car a)
        (let ((ck (Arith-div arith
                             (car c)
                             (Arith-sub arith
                                        (car b)
                                        (Arith-mul arith
                                                   (car a)
                                                   (car k)))))
              (cb (Arith-div arith
                             (Arith-add arith
                                        (car d)
                                        (Arith-mul arith
                                                   (car a)
                                                   (cadr k)))
                             (Arith-sub arith
                                        (car b)
                                        (Arith-mul arith
                                                   (car a)
                                                   (car k))))))
          (runthrough-k-run (cdr a) (cdr b) (cdr c) (cdr d) end
                            (cons (list ck cb) k-stack) arith))
        (let ((res (cons (Arith-div arith
                                    (Arith-sub arith
                                               (InitialValue-p-val end)
                                               (Arith-mul arith
                                                          (InitialValue-m-val end)
                                                          (cadr k)))
                                    (Arith-add arith
                                               (InitialValue-k-val end)
                                               (Arith-mul arith
                                                          (InitialValue-m-val end)
                                                          (car k))))
                         nil)))
          (runthrough-back-run k-stack res arith)))))

(defun runthrough-inner (matrix start end arith)
  (let ((ck (Arith-div arith
                       (Arith-sub arith (InitialValue-k-val start))
                       (InitialValue-m-val start)))
        (cb (Arith-div arith
                       (InitialValue-p-val start)
                       (InitialValue-m-val start))))
    (runthrough-k-run (Matrix3D-A matrix) (Matrix3D-B matrix) (Matrix3D-C matrix)
                      (Matrix3D-D matrix) end (cons (list ck cb) nil) arith)))

(defun runthrough (matrix start end arith)
  (cond ((not (Arith-p arith)) (error (make-condition 'NOT-ARITH)))
        ((not (Matrix3D-p matrix)) (error (make-condition 'NOT-MATRIX3D)))
        ((not (Matrix3D-check matrix)) (error (make-condition 'INVALID-MATRIX3D)))
        ((not (InitialValue-p start)) (error (make-condition 'NOT-INITIALVALUE)))
        ((not (InitialValue-p end)) (error (make-condition 'NOT-INITIALVALUE)))
        ((runthrough-inner matrix start end arith))))

