(load "arith.lisp")
(load "runthrough.lisp")

(defstruct TaskHandle
  start               ;;; handle -> (timeline z-line)
  get-coefficients    ;;; handle -> ((z specific A B C D))
  step                ;;; handle res -> T | Nil
  step-t              ;;; handle res -> T | Nil
  get-start           ;;; handle -> InitialValue start
  get-end             ;;; handle -> InitialValue end
  iter-limit          ;;; step iteration limit (used if not nil)
  arg                 ;;; task specific handle
  arith)

(defun solve-step (handle n)
  (let* ((matrix-raw (apply 'mapcar (lambda (&rest x) x) (funcall (TaskHandle-get-coefficients handle) handle))) ;;; ~transpose
         (matrix (make-Matrix3D :size (length (car matrix-raw))
                                :A    (car matrix-raw)
                                :B    (cadr matrix-raw)
                                :C    (caddr matrix-raw)
                                :D    (cadddr matrix-raw)))
         (res (runthrough matrix
                          (funcall (TaskHandle-get-start handle) handle)
                          (funcall (TaskHandle-get-end handle) handle)
                          (TaskHandle-arith handle)))
         (nn (if (not n) n (Arith-sub (TaskHandle-arith handle) n 1))))
    (cond ((and n (= 0 n)) res)
          ((funcall (TaskHandle-step handle) handle res) (solve-step handle nn))
          (res))))

(defun solve-time-step (handle temp)
  (let* ((res (solve-step handle (TaskHandle-iter-limit handle)))
         (c-temp (cons res temp)))
    (if (funcall (TaskHandle-step-t handle) handle res)
        (solve-time-step handle c-temp)
        c-temp)))

(defun solve (handle)
  (let ((res (funcall (TaskHandle-start handle) handle)))
    (list (car res) (cadr res) (nreverse (solve-time-step handle nil)))))

