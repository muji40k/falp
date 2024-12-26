(load "arith.lisp")
(load "runthrough.lisp")

(defstruct TaskHandle
  start               ;;; handle -> (timeline z-line ihandle)
  get-coefficients    ;;; handle ihandle -> ((z specific A B C D))
  step                ;;; handle ihandle res -> ihandle
  step-t              ;;; handle ihandle res -> ihandle
  continue            ;;; ihandle -> T | Nil
  get-start           ;;; handle ihandle -> InitialValue start
  get-end             ;;; handle ihandle -> InitialValue end
  iter-limit          ;;; step iteration limit (used if not nil)
  arg                 ;;; task specific handle
  arith)

(defun solve-step (handle ihandle n)
  (let* ((matrix-raw (apply 'mapcar (lambda (&rest x) x) (funcall (TaskHandle-get-coefficients handle) handle ihandle))) ;;; ~transpose
         (matrix (make-Matrix3D :size (length (car matrix-raw))
                                :A    (car matrix-raw)
                                :B    (cadr matrix-raw)
                                :C    (caddr matrix-raw)
                                :D    (cadddr matrix-raw)))
         (res (runthrough matrix
                          (funcall (TaskHandle-get-start handle) handle ihandle)
                          (funcall (TaskHandle-get-end handle) handle ihandle)
                          (TaskHandle-arith handle)))
         (nn (if (not n) n (Arith-sub (TaskHandle-arith handle) n 1)))
         (iter (funcall (TaskHandle-step handle) handle ihandle res)))
    (cond ((and n (= 0 n)) (list res iter))
          ((funcall (TaskHandle-continue handle) iter) (solve-step handle iter nn))
          ((list res iter)))))

(defun solve-time-step (handle ihandle temp)
  (let* ((res (solve-step handle ihandle (TaskHandle-iter-limit handle)))
         (c-temp (cons (car res) temp))
         (iter (funcall (TaskHandle-step-t handle) handle (cadr res) (car res))))
    (if (funcall (TaskHandle-continue handle) iter)
        (solve-time-step handle iter c-temp)
        c-temp)))

(defun solve (handle)
  (let ((res (funcall (TaskHandle-start handle) handle)))
    (list (car res) (cadr res) (nreverse (solve-time-step handle (caddr res) nil)))))

