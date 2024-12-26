(load "misc.lisp")
(load "common_task.lisp")

(defun get-func-k ()
  (get-table-func '(293 1278 1528 1677 2000 2400)
                  '(2.0e-2 5.0e-2 7.8e-2 1.0e-1 1.3e-1 2.0e-1)))

(defun get-func-f (fmax tmax v arith)
  (func-trans (lambda (x arith-in) (Arith-mul arith-in
                                              (Arith-exp arith-in
                                                         (Arith-sub arith-in
                                                                    1
                                                                    (Arith-div arith-in
                                                                               x
                                                                               tmax)))
                                              x))
              (Arith-div arith fmax tmax)
              0
              (Arith-div arith 1 v)))

(defun get-func-c (a b c m)
  (lambda (x arith) (Arith-add arith
                               a
                               (Arith-sub arith
                                          (Arith-mul arith
                                                     b
                                                     (Arith-pow arith x m))
                                          (Arith-div arith
                                                     c
                                                     (Arith-mul arith x x))))))

(defun get-func-lambda ()
  (get-table-func '(300 500 800 1100 2000 2400)
                  '(1.36e-2 1.63e-2 1.81e-2 1.98e-2 2.50e-2 2.74e-2)))

(defstruct InnerHandle step-z step-t grid-z current-t current-f
  temp-prev temp-prev-t ir irsqrh basep basef at0ir)

(defstruct CylinderTaskHandle
  _k _f _c _lambda n t0 alpha r0 r_max r_steps time_start time_end time_step
  eps handle)

(defun print-ci-handle (handle)
  (print (InnerHandle-step-z handle))
  (print (InnerHandle-step-t handle))
  ;;; (print (InnerHandle-grid-z handle))
  (print (InnerHandle-current-t handle))
  (print (InnerHandle-current-f handle))
  ;;; (print (InnerHandle-temp-prev handle))
  ;;; (print (InnerHandle-temp-prev-t handle))
  (print (InnerHandle-ir handle))
  (print (InnerHandle-irsqrh handle))
  (print (InnerHandle-basep handle))
  (print (InnerHandle-basef handle))
  (print (InnerHandle-at0ir handle)))

(defun print-c-handle (handle)
  (print (CylinderTaskHandle-_k handle))
  (print (CylinderTaskHandle-_f handle))
  (print (CylinderTaskHandle-_c handle))
  (print (CylinderTaskHandle-_lambda handle))
  (print (CylinderTaskHandle-n handle))
  (print (CylinderTaskHandle-t0 handle))
  (print (CylinderTaskHandle-alpha handle))
  (print (CylinderTaskHandle-r0 handle))
  (print (CylinderTaskHandle-r_max handle))
  (print (CylinderTaskHandle-r_steps handle))
  (print (CylinderTaskHandle-time_start handle))
  (print (CylinderTaskHandle-time_end handle))
  (print (CylinderTaskHandle-time_step handle))
  (print (CylinderTaskHandle-eps handle))
  (print-ci-handle (CylinderTaskHandle-handle handle)))

(defun CylinderTaskHandle-k (handle temp arith) (funcall (CylinderTaskHandle-_k handle) temp arith))
(defun CylinderTaskHandle-f (handle time arith) (funcall (CylinderTaskHandle-_f handle) time arith))
(defun CylinderTaskHandle-c (handle temp arith) (funcall (CylinderTaskHandle-_c handle) temp arith))
(defun CylinderTaskHandle-lambda (handle temp arith) (funcall (CylinderTaskHandle-_lambda handle) temp arith))

(defun CylinderTaskHandle-start (handle)
  (let* ((c-handle (TaskHandle-arg handle))
         (ci-handle (make-InnerHandle))
         (arith (TaskHandle-arith handle))
         (origin-grid-z (linspace (CylinderTaskHandle-r0 c-handle)
                                  (CylinderTaskHandle-r_max c-handle)
                                  (Arith-add arith (CylinderTaskHandle-r_steps c-handle) 1)
                                  arith))
         (grid-z (mapcar (lambda (x)
                           (Arith-div arith
                                      x
                                      (CylinderTaskHandle-r_max c-handle)))
                         origin-grid-z))
         (grid-t (linspace (CylinderTaskHandle-time_start c-handle)
                           (CylinderTaskHandle-time_end c-handle)
                           (Arith-add arith (CylinderTaskHandle-time_step c-handle) 1)
                           arith)))
    (setf (CylinderTaskHandle-handle c-handle) ci-handle)
    (setf (InnerHandle-step-z ci-handle)
          (Arith-div arith
                     (Arith-sub arith
                                (CylinderTaskHandle-r_max c-handle)
                                (CylinderTaskHandle-r0 c-handle))
                     (Arith-mul arith
                                (CylinderTaskHandle-r_steps c-handle)
                                (CylinderTaskHandle-r_max c-handle))))
    (setf (InnerHandle-step-t ci-handle)
          (Arith-div arith
                     (Arith-sub arith
                                (CylinderTaskHandle-time_end c-handle)
                                (CylinderTaskHandle-time_start c-handle))
                     (CylinderTaskHandle-time_step c-handle)))
    (setf (InnerHandle-grid-z ci-handle) grid-z)
    (setf (InnerHandle-current-t ci-handle)
          (CylinderTaskHandle-time_start c-handle))
    (setf (InnerHandle-current-f ci-handle)
          (CylinderTaskHandle-f c-handle
                                (CylinderTaskHandle-time_start c-handle)
                                arith))
    (setf (InnerHandle-temp-prev ci-handle)
          (make-list (floor (Arith-add arith (CylinderTaskHandle-r_steps c-handle) 1))
                     :initial-element (CylinderTaskHandle-t0 c-handle)))
    (setf (InnerHandle-temp-prev-t ci-handle)
          (make-list (floor (Arith-add arith (CylinderTaskHandle-r_steps c-handle) 1))
                     :initial-element (CylinderTaskHandle-t0 c-handle)))
    (setf (InnerHandle-ir ci-handle)
          (Arith-div arith 1 (CylinderTaskHandle-r_max c-handle)))
    (setf (InnerHandle-irsqrh ci-handle)
          (Arith-div arith 1 (Arith-mul arith
                                        (CylinderTaskHandle-r_max c-handle)
                                        (CylinderTaskHandle-r_max c-handle)
                                        (InnerHandle-step-z ci-handle))))
    (setf (InnerHandle-basep ci-handle)
          (Arith-mul arith
                     (CylinderTaskHandle-n c-handle)
                     (CylinderTaskHandle-n c-handle)
                     4 5.668e-12))
    (setf (InnerHandle-basef ci-handle)
          (Arith-mul arith
                     (InnerHandle-basep ci-handle)
                     (Arith-pow arith (CylinderTaskHandle-t0 c-handle) 4)))
    (setf (InnerHandle-at0ir ci-handle)
          (Arith-div arith
                     (Arith-mul arith
                                (CylinderTaskHandle-alpha c-handle)
                                (CylinderTaskHandle-t0 c-handle))
                     (CylinderTaskHandle-r_max c-handle)))
    ;;; (print-c-handle c-handle)
    (list grid-t origin-grid-z)))

(defun get-coefficients (grid temp-prev temp-prev-t c-handle ci-handle arith tmp)
  (if (not (caddr grid)) tmp
      (let* ((zm (mid (car grid) (cadr grid) arith))
             (zp (mid (cadr grid) (caddr grid) arith))
             (v (Arith-div arith
                           (Arith-sub arith
                                      (Arith-mul arith zp zp)
                                      (Arith-mul arith zm zm))
                           2))
             (kappam (mid (CylinderTaskHandle-lambda c-handle (car temp-prev) arith)
                          (CylinderTaskHandle-lambda c-handle (cadr temp-prev) arith)
                          arith))
             (kappap (mid (CylinderTaskHandle-lambda c-handle (cadr temp-prev) arith)
                          (CylinderTaskHandle-lambda c-handle (caddr temp-prev) arith)
                          arith))
             (k (CylinderTaskHandle-k c-handle (cadr temp-prev) arith))
             (p (Arith-mul arith
                           (InnerHandle-basep ci-handle)
                           k
                           (Arith-pow arith (cadr temp-prev) 3)
                           (InnerHandle-step-t ci-handle)))
             (f (Arith-mul arith
                           (InnerHandle-basef ci-handle)
                           k
                           (InnerHandle-step-t ci-handle)))
             (ch (Arith-mul arith
                            (CylinderTaskHandle-c c-handle (cadr temp-prev) arith)
                            (InnerHandle-step-z ci-handle)))
             (a (Arith-mul arith
                           (InnerHandle-irsqrh ci-handle)
                           zm
                           kappam
                           (InnerHandle-step-t ci-handle)))
             (c (Arith-mul arith
                           (InnerHandle-irsqrh ci-handle)
                           zp
                           kappap
                           (InnerHandle-step-t ci-handle)))
             (b (Arith-add arith
                           a c
                           (Arith-mul arith p v)
                           ch))
             (d (Arith-add arith
                           (Arith-mul arith f v)
                           (Arith-mul arith ch (cadr temp-prev-t)))))
        (get-coefficients (cdr grid) (cdr temp-prev) (cdr temp-prev-t)
                          c-handle ci-handle arith
                          (cons (list a b c d) tmp)))))

(defun CylinderTaskHandle-get-coefficients (handle)
  (let* ((c-handle (TaskHandle-arg handle))
         (ci-handle (CylinderTaskHandle-handle c-handle)))
    (nreverse (get-coefficients (InnerHandle-grid-z ci-handle)
                                (InnerHandle-temp-prev ci-handle)
                                (InnerHandle-temp-prev-t ci-handle)
                                c-handle
                                ci-handle
                                (TaskHandle-arith handle)
                                nil))))

(defun CylinderTaskHandle-step (handle res)
  (let* ((c-handle (TaskHandle-arg handle))
         (ci-handle (CylinderTaskHandle-handle c-handle))
         (arith (TaskHandle-arith handle))
         (prev (InnerHandle-temp-prev ci-handle)))
    (setf (InnerHandle-temp-prev ci-handle) res)
    (< (CylinderTaskHandle-eps c-handle) (apply 'max (mapcar (lambda (x y)
                                                               (abs (Arith-div arith (Arith-sub arith x y) y)))
                                                             prev res)))))

(defun CylinderTaskHandle-step-t (handle res)
  (let* ((c-handle (TaskHandle-arg handle))
         (ci-handle (CylinderTaskHandle-handle c-handle))
         (arith (TaskHandle-arith handle))
         (ctime (Arith-add arith
                           (InnerHandle-current-t ci-handle)
                           (InnerHandle-step-t ci-handle))))
    (setf (InnerHandle-temp-prev ci-handle) res)
    (setf (InnerHandle-temp-prev-t ci-handle) res)
    (setf (InnerHandle-current-t ci-handle) ctime)
    (setf (InnerHandle-current-f ci-handle)
          (CylinderTaskHandle-f c-handle ctime arith))
    (<= (InnerHandle-current-t ci-handle)
        (CylinderTaskHandle-time_end c-handle))))

(defun CylinderTaskHandle-get-start (handle)
  (let* ((c-handle (TaskHandle-arg handle))
         (ci-handle (CylinderTaskHandle-handle c-handle))
         (arith (TaskHandle-arith handle))
         (grid (InnerHandle-grid-z ci-handle))
         (prev (InnerHandle-temp-prev ci-handle))
         (prev-t (InnerHandle-temp-prev-t ci-handle))
         (zhl (mid (car grid) (cadr grid) arith))
         (kappal (mid (CylinderTaskHandle-lambda c-handle (car prev) arith)
                      (CylinderTaskHandle-lambda c-handle (cadr prev) arith)
                      arith))
         (k0 (CylinderTaskHandle-k c-handle (car prev) arith))
         (k1 (CylinderTaskHandle-k c-handle (cadr prev) arith))
         (p0 (Arith-mul arith
                        (InnerHandle-basep ci-handle)
                        k0
                        (Arith-pow arith (car prev) 3)))
         (p1 (Arith-mul arith
                        (InnerHandle-basep ci-handle)
                        k1
                        (Arith-pow arith (cadr prev) 3)))
         (pl (mid p0 p1 arith))
         (f0 (Arith-mul arith (InnerHandle-basef ci-handle) k0))
         (f1 (Arith-mul arith (InnerHandle-basef ci-handle) k1))
         (fl (mid f0 f1 arith))
         (ch (Arith-mul arith
                        (CylinderTaskHandle-c c-handle (car prev) arith)
                        (InnerHandle-step-z ci-handle)
                        0.5))
         (m (Arith-add arith
                       ch
                       (Arith-mul arith
                                  (InnerHandle-step-t ci-handle)
                                  (Arith-add arith
                                             (Arith-mul arith
                                                        zhl
                                                        (Arith-add arith
                                                                   (Arith-mul arith
                                                                              kappal
                                                                              (InnerHandle-irsqrh ci-handle))
                                                                   (Arith-mul arith
                                                                              (InnerHandle-step-z ci-handle)
                                                                              pl
                                                                              0.125)))
                                             (Arith-mul arith
                                                        (InnerHandle-step-z ci-handle)
                                                        p0
                                                        (car prev)
                                                        0.25)))))
         (k (Arith-mul arith
                       zhl
                       (InnerHandle-step-t ci-handle)
                       (Arith-sub arith
                                  (Arith-mul arith
                                             (InnerHandle-step-z ci-handle)
                                             pl
                                             0.125)
                                  (Arith-mul arith
                                             kappal
                                             (InnerHandle-irsqrh ci-handle)))))
         (p (Arith-add arith
                       (Arith-mul arith ch (car prev-t))
                       (Arith-mul arith
                                  (InnerHandle-step-t ci-handle)
                                  (Arith-add arith
                                             (Arith-mul arith
                                                        (car grid)
                                                        (InnerHandle-current-f ci-handle)
                                                        (InnerHandle-ir ci-handle))
                                             (Arith-mul arith
                                                        (InnerHandle-step-z ci-handle)
                                                        0.25
                                                        (Arith-add arith
                                                                   (Arith-mul arith f0 (car grid))
                                                                   (Arith-mul arith fl zhl))))))))
    (make-InitialValue :m-val m :k-val k :p-val p)))

(defun CylinderTaskHandle-get-end (handle)
  (let* ((c-handle (TaskHandle-arg handle))
         (ci-handle (CylinderTaskHandle-handle c-handle))
         (arith (TaskHandle-arith handle))
         (rgrid (reverse (InnerHandle-grid-z ci-handle)))
         (rprev (reverse (InnerHandle-temp-prev ci-handle)))
         (rprev-t (reverse (InnerHandle-temp-prev-t ci-handle)))
         (zhh (mid (car rgrid) (cadr rgrid) arith))
         (kappah (mid (CylinderTaskHandle-lambda c-handle (car rprev) arith)
                      (CylinderTaskHandle-lambda c-handle (cadr rprev) arith)
                      arith))
         (kN0 (CylinderTaskHandle-k c-handle (cadr rprev) arith))
         (kN1 (CylinderTaskHandle-k c-handle (car rprev) arith))
         (pN0 (Arith-mul arith
                        (InnerHandle-basep ci-handle)
                        kN0
                        (Arith-pow arith (cadr rprev) 3)))
         (pN1 (Arith-mul arith
                        (InnerHandle-basep ci-handle)
                        kN1
                        (Arith-pow arith (car rprev) 3)))
         (ph (mid pN0 pN1 arith))
         (fN0 (Arith-mul arith (InnerHandle-basef ci-handle) kN0))
         (fN1 (Arith-mul arith (InnerHandle-basef ci-handle) kN1))
         (fh (mid fN0 fN1 arith))
         (ch (Arith-mul arith
                        (CylinderTaskHandle-c c-handle (car rprev) arith)
                        (InnerHandle-step-z ci-handle)
                        0.5))
         (m (Arith-mul arith
                       zhh
                       (InnerHandle-step-t ci-handle)
                       (Arith-sub arith
                                  (Arith-mul arith
                                             (InnerHandle-step-z ci-handle)
                                             ph
                                             0.125)
                                  (Arith-mul arith
                                             kappah
                                             (InnerHandle-irsqrh ci-handle)))))
         (k (Arith-add arith
                       ch
                       (Arith-mul arith
                                  (InnerHandle-step-t ci-handle)
                                  (Arith-add arith
                                             (Arith-mul arith kappah zhh (InnerHandle-irsqrh ci-handle))
                                             (Arith-mul arith (car rgrid) (CylinderTaskHandle-alpha c-handle) (InnerHandle-ir ci-handle))
                                             (Arith-mul arith
                                                        0.25
                                                        (InnerHandle-step-z ci-handle)
                                                        (Arith-add arith
                                                                   (Arith-mul arith ph zhh 0.5)
                                                                   (Arith-mul arith pN1 (car rgrid))))))))
         (p (Arith-add arith
                       (Arith-mul arith ch (car rprev-t))
                       (Arith-mul arith
                                  (InnerHandle-step-t ci-handle)
                                  (Arith-add arith
                                             (Arith-mul arith
                                                        (car rgrid)
                                                        (InnerHandle-at0ir ci-handle))
                                             (Arith-mul arith
                                                        (InnerHandle-step-z ci-handle)
                                                        0.25
                                                        (Arith-add arith
                                                                   (Arith-mul arith fN1 (car rgrid))
                                                                   (Arith-mul arith fh zhh))))))))
    (make-InitialValue :m-val m :k-val k :p-val p)))

(defun SetupTaskHandleCylinder (handle _k _f _c _lambda n t0 alpha r0 r_max
                                  r_steps time_start time_end time_step eps
                                  arith)

  (setf (TaskHandle-start handle)            #'CylinderTaskHandle-start)
  (setf (TaskHandle-get-coefficients handle) #'CylinderTaskHandle-get-coefficients)
  (setf (TaskHandle-step handle)             #'CylinderTaskHandle-step)
  (setf (TaskHandle-step-t handle)           #'CylinderTaskHandle-step-t)
  (setf (TaskHandle-get-start handle)        #'CylinderTaskHandle-get-start)
  (setf (TaskHandle-get-end handle)          #'CylinderTaskHandle-get-end)
  (setf (TaskHandle-iter-limit handle)       1000)
  (setf (TaskHandle-arg handle)              (make-CylinderTaskHandle :_k _k
                                                                      :_f _f
                                                                      :_c _c
                                                                      :_lambda _lambda
                                                                      :n n
                                                                      :t0 t0
                                                                      :alpha alpha
                                                                      :r0 r0
                                                                      :r_max r_max
                                                                      :r_steps r_steps
                                                                      :time_start time_start
                                                                      :time_end time_end
                                                                      :time_step time_step
                                                                      :eps eps))
  (setf (TaskHandle-arith handle)            arith))

(defun FormTaskHandleCylinder (_k _f _c _lambda n t0 alpha r0 r_max
                                  r_steps time_start time_end time_step eps
                                  arith)
  (make-TaskHandle :start            #'CylinderTaskHandle-start
                   :get-coefficients #'CylinderTaskHandle-get-coefficients
                   :step             #'CylinderTaskHandle-step
                   :step-t           #'CylinderTaskHandle-step-t
                   :get-start        #'CylinderTaskHandle-get-start
                   :get-end          #'CylinderTaskHandle-get-end
                   :iter-limit       1000
                   :arg              (make-CylinderTaskHandle :_k _k
                                                              :_f _f
                                                              :_c _c
                                                              :_lambda _lambda
                                                              :n n
                                                              :t0 t0
                                                              :alpha alpha
                                                              :r0 r0
                                                              :r_max r_max
                                                              :r_steps r_steps
                                                              :time_start time_start
                                                              :time_end time_end
                                                              :time_step time_step
                                                              :eps eps)
                                                              ;;; :eps eps
                                                              ;;; :handle (make-InnerHandle))
                   :arith            arith))

