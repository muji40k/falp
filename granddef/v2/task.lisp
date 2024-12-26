(load "misc.lisp")
(load "common_task.lisp")

(defun get-func-k ()
  (get-table-func '(293 1278 1528 1677 2000 2400)
                  '(2.0e-2 5.0e-2 7.8e-2 1.0e-1 1.3e-1 2.0e-1)))

;;; (defun get-func-f (fmax tmax v arith) ; const
;;;   (lambda (_ __) fmax))

;;; (defun get-func-f (fmax tmax v arith) ; saw
;;;   (func-trans (lambda (x arith-in) (Arith-mul arith-in x v))
;;;               fmax
;;;               0
;;;               (Arith-div arith 1 v)))


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

(defstruct IterHandle temp-prev temp-prev-t current-t current-f stop)

(defstruct InnerHandle step-z step-t grid-z ir irsqrh basep basef at0ir)

(defstruct CylinderTaskHandle _k _f _c _lambda n t0 alpha r0 r_max r_steps
  time_start time_end time_step eps)

(defun CylinderTaskHandle-k (handle temp arith) (funcall (CylinderTaskHandle-_k handle) temp arith))
(defun CylinderTaskHandle-f (handle time arith) (funcall (CylinderTaskHandle-_f handle) time arith))
(defun CylinderTaskHandle-c (handle temp arith) (funcall (CylinderTaskHandle-_c handle) temp arith))
(defun CylinderTaskHandle-lambda (handle temp arith) (funcall (CylinderTaskHandle-_lambda handle) temp arith))

(defun CylinderTaskHandle-start (handle)
  (let* ((c-handle (TaskHandle-arg handle))
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
                           arith))
         (step-z (Arith-div arith
                            (Arith-sub arith
                                       (CylinderTaskHandle-r_max c-handle)
                                       (CylinderTaskHandle-r0 c-handle))
                            (Arith-mul arith
                                       (CylinderTaskHandle-r_steps c-handle)
                                       (CylinderTaskHandle-r_max c-handle))))
         (basep (Arith-mul arith
                           (CylinderTaskHandle-n c-handle)
                           (CylinderTaskHandle-n c-handle)
                           4 5.668e-12))
         (iter (cons (make-InnerHandle :step-z step-z
                                       :step-t (Arith-div arith
                                                          (Arith-sub arith
                                                                     (CylinderTaskHandle-time_end c-handle)
                                                                     (CylinderTaskHandle-time_start c-handle))
                                                          (CylinderTaskHandle-time_step c-handle))
                                       :grid-z grid-z
                                       :ir (Arith-div arith 1 (CylinderTaskHandle-r_max c-handle))
                                       :irsqrh (Arith-div arith 1 (Arith-mul arith
                                                                             (CylinderTaskHandle-r_max c-handle)
                                                                             (CylinderTaskHandle-r_max c-handle)
                                                                             step-z))
                                       :basep basep
                                       :basef (Arith-mul arith
                                                         basep
                                                         (Arith-pow arith (CylinderTaskHandle-t0 c-handle) 4))
                                       :at0ir (Arith-div arith
                                                         (Arith-mul arith
                                                                    (CylinderTaskHandle-alpha c-handle)
                                                                    (CylinderTaskHandle-t0 c-handle))
                                                         (CylinderTaskHandle-r_max c-handle)))
                     (make-IterHandle :temp-prev   (make-list (floor (Arith-add arith (CylinderTaskHandle-r_steps c-handle) 1))
                                                              :initial-element (CylinderTaskHandle-t0 c-handle))
                                      :temp-prev-t (make-list (floor (Arith-add arith (CylinderTaskHandle-r_steps c-handle) 1))
                                                              :initial-element (CylinderTaskHandle-t0 c-handle))
                                      :current-t (CylinderTaskHandle-time_start c-handle)
                                      :current-f (CylinderTaskHandle-f c-handle
                                                                       (CylinderTaskHandle-time_start c-handle)
                                                                       arith)
                                      :stop nil))))
    (list grid-t origin-grid-z iter)))

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

(defun CylinderTaskHandle-get-coefficients (handle ihandle)
  (let* ((c-handle (TaskHandle-arg handle))
         (ci-handle (car ihandle))
         (cit-handle (cdr ihandle)))
    (nreverse (get-coefficients (InnerHandle-grid-z ci-handle)
                                (IterHandle-temp-prev cit-handle)
                                (IterHandle-temp-prev-t cit-handle)
                                c-handle
                                ci-handle
                                (TaskHandle-arith handle)
                                nil))))

(defun CylinderTaskHandle-step (handle ihandle res)
  (let* ((c-handle (TaskHandle-arg handle))
         (ci-handle (car ihandle))
         (cit-handle (cdr ihandle))
         (arith (TaskHandle-arith handle))
         (prev (IterHandle-temp-prev cit-handle)))
    (cons ci-handle
          (make-IterHandle :temp-prev res
                           :temp-prev-t (IterHandle-temp-prev-t cit-handle)
                           :current-t (IterHandle-current-t cit-handle)
                           :current-f (IterHandle-current-f cit-handle)
                           :stop (> (CylinderTaskHandle-eps c-handle)
                                    (apply 'max (mapcar (lambda (x y)
                                                          (abs (Arith-div arith (Arith-sub arith x y) y)))
                                                        prev res)))))))

(defun CylinderTaskHandle-step-t (handle ihandle res)
  (let* ((c-handle (TaskHandle-arg handle))
         (ci-handle (car ihandle))
         (cit-handle (cdr ihandle))
         (arith (TaskHandle-arith handle))
         (ctime (Arith-add arith
                           (IterHandle-current-t cit-handle)
                           (InnerHandle-step-t ci-handle))))
    (cons ci-handle
          (make-IterHandle :temp-prev res
                           :temp-prev-t res
                           :current-t ctime
                           :current-f (CylinderTaskHandle-f c-handle ctime arith)
                           :stop (> ctime
                                    (CylinderTaskHandle-time_end c-handle))))))

(defun CylinderTaskHandle-get-start (handle ihandle)
  (let* ((c-handle (TaskHandle-arg handle))
         (ci-handle (car ihandle))
         (cit-handle (cdr ihandle))
         (arith (TaskHandle-arith handle))
         (grid (InnerHandle-grid-z ci-handle))
         (prev (IterHandle-temp-prev cit-handle))
         (prev-t (IterHandle-temp-prev-t cit-handle))
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
                                                        (IterHandle-current-f cit-handle)
                                                        (InnerHandle-ir ci-handle))
                                             (Arith-mul arith
                                                        (InnerHandle-step-z ci-handle)
                                                        0.25
                                                        (Arith-add arith
                                                                   (Arith-mul arith f0 (car grid))
                                                                   (Arith-mul arith fl zhl))))))))
    (make-InitialValue :m-val m :k-val k :p-val p)))

;;; (defun CylinderTaskHandle-get-end (handle ihandle)
;;;     (make-InitialValue :m-val 0 :k-val 1 :p-val 600))

(defun CylinderTaskHandle-get-end (handle ihandle)
  (let* ((c-handle (TaskHandle-arg handle))
         (ci-handle (car ihandle))
         (cit-handle (cdr ihandle))
         (arith (TaskHandle-arith handle))
         (rgrid (reverse (InnerHandle-grid-z ci-handle)))
         (rprev (reverse (IterHandle-temp-prev cit-handle)))
         (rprev-t (reverse (IterHandle-temp-prev-t cit-handle)))
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

(defun CylinderTaskHandle-continue (ihandle)
  (not (IterHandle-stop (cdr ihandle))))

(defun FormTaskHandleCylinder (_k _f _c _lambda n t0 alpha r0 r_max
                                  r_steps time_start time_end time_step eps
                                  arith)
  (make-TaskHandle :start            #'CylinderTaskHandle-start
                   :get-coefficients #'CylinderTaskHandle-get-coefficients
                   :step             #'CylinderTaskHandle-step
                   :step-t           #'CylinderTaskHandle-step-t
                   :continue         #'CylinderTaskHandle-continue
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
                   :arith            arith))

