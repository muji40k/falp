(ql:quickload "cl-cffi-gtk")

(load "task.lisp")
(load "my-arith.lisp")

(defvar arith (make-Arith :_add '+ :_sub '- :_mul '* :_div '/))
(defvar arith-list (list (cons "Системная" arith)
                         (cons "Собственная" (make-list-arith))))

(defun construct-f-func (builder)
  (get-func-f
    (gtk:gtk-adjustment-get-value (gtk:gtk-builder-get-object builder "F_max_adj"))
    (gtk:gtk-adjustment-get-value (gtk:gtk-builder-get-object builder "t_max_adj"))
    (gtk:gtk-adjustment-get-value (gtk:gtk-builder-get-object builder "v_adj"))
    arith))

(defun construct-c-func (builder)
  (get-func-c
    (gtk:gtk-adjustment-get-value (gtk:gtk-builder-get-object builder "a_adj"))
    (Arith-mul arith
               (gtk:gtk-adjustment-get-value (gtk:gtk-builder-get-object builder "b_adj"))
               (Arith-pow arith 10 (floor (gtk:gtk-adjustment-get-value (gtk:gtk-builder-get-object builder "b_eps_adj")))))
    (Arith-mul arith
               (gtk:gtk-adjustment-get-value (gtk:gtk-builder-get-object builder "c_adj"))
               (Arith-pow arith 10 (floor (gtk:gtk-adjustment-get-value (gtk:gtk-builder-get-object builder "c_eps_adj")))))
    (floor (gtk:gtk-adjustment-get-value (gtk:gtk-builder-get-object builder "m_adj")))))

(defun construct-task-handle (handle builder)
  (SetupTaskHandleCylinder handle
                           (get-func-k)
                           (construct-f-func builder)
                           (construct-c-func builder)
                           (get-func-lambda)
                           (gtk:gtk-adjustment-get-value (gtk:gtk-builder-get-object builder "n_adj"))
                           (gtk:gtk-adjustment-get-value (gtk:gtk-builder-get-object builder "T0_adj"))
                           (gtk:gtk-adjustment-get-value (gtk:gtk-builder-get-object builder "alpha_adj"))
                           (gtk:gtk-adjustment-get-value (gtk:gtk-builder-get-object builder "r0_adj"))
                           (gtk:gtk-adjustment-get-value (gtk:gtk-builder-get-object builder "R_adj"))
                           (floor (gtk:gtk-adjustment-get-value (gtk:gtk-builder-get-object builder "steps_z")))
                           0
                           (gtk:gtk-adjustment-get-value (gtk:gtk-builder-get-object builder "t_end_adj"))
                           (floor (gtk:gtk-adjustment-get-value (gtk:gtk-builder-get-object builder "steps_t")))
                           (Arith-pow arith 10 (floor (gtk:gtk-adjustment-get-value (gtk:gtk-builder-get-object builder "eps_adj"))))
                           (construct-arith builder)))

(defun construct-arith-inner (item lst)
  (cond ((not lst) arith)
        ((equal item (car (car lst))) (cdr (car lst)))
        ((construct-arith-inner item (cdr lst)))))

(defun construct-arith (builder)
  (let ((item (gtk:gtk-entry-text (gtk:gtk-builder-get-object builder "combo_box_text"))))
    (construct-arith-inner item arith-list)))

(defun get-draw-wrap (callback arg)
  (lambda (widget cr)
    (let* ((cr (gobject:pointer cr))
           (width (gtk:gtk-widget-get-allocated-width widget))
           (height (gtk:gtk-widget-get-allocated-height widget))
           (context (gtk:gtk-widget-get-style-context widget))
           (color (gtk:gtk-style-context-get-color context :focused)))
      (gdk:gdk-cairo-set-source-rgba cr color)
      (funcall callback cr width height arg)
      (cairo:cairo-fill cr))))

(defstruct ScreenTransform width height w-offset h-offset x-scale y-scale x-min y-min)

(defun ScreenTransform-transform (handle x y)
  (list x (Arith-sub arith (ScreenTransform-height handle) y)))
(defun ScreenTransform-transform-graph (handle x y)
  (list (Arith-add arith
                   (ScreenTransform-w-offset handle)
                   (Arith-mul arith
                              (ScreenTransform-x-scale handle)
                              (Arith-sub arith
                                         x
                                         (ScreenTransform-x-min handle))))
        (Arith-add arith
                   (ScreenTransform-h-offset handle)
                   (Arith-mul arith
                              (ScreenTransform-y-scale handle)
                              (Arith-sub arith
                                         y
                                         (ScreenTransform-y-min handle))))))

(defun default-map (w)
  (let* ((w (cond ((< w 0) 0)
                  ((< 1 w) 1)
                  (w)))
         (frac (Arith-div arith (Arith-mod arith w 0.25) 0.25)))
    (cond ((< w 0) '(0 0 1))
          ((< w 0.25) (list 0 frac 1))
          ((< w 0.5) (list 0 1 (Arith-sub arith 1 frac)))
          ((< w 0.75) (list frac 1 0))
          ((< w 1) (list 1 (Arith-sub arith 1 frac) 0))
          ('(1 0 0)))))

(defun draw-title (cr handle title pos)
  (if (not (null title))
      (let ((extents (cairo:cairo-text-extents cr title))
            (woff (Arith-mul arith (ScreenTransform-width handle) 0.05))
            (hoff (Arith-mul arith (ScreenTransform-height handle) 0.05)))
        (cond ((equal 'x pos)
               (apply 'cairo:cairo-move-to cr
                      (ScreenTransform-transform handle
                                                 (Arith-sub arith
                                                            (Arith-div arith (ScreenTransform-width handle) 2)
                                                            (Arith-div arith (cairo:cairo-text-extents-t-width extents) 2))
                                                 (Arith-sub arith
                                                            hoff
                                                            (Arith-div arith (cairo:cairo-text-extents-t-height extents) 2))))
               (cairo:cairo-show-text cr title))
              ((equal 'y pos)
               (cairo:cairo-save cr)
               (apply 'cairo:cairo-move-to cr
                      (ScreenTransform-transform handle
                                                 (Arith-add arith
                                                            woff
                                                            (Arith-div arith (cairo:cairo-text-extents-t-height extents) 2))
                                                 (Arith-sub arith
                                                            (Arith-div arith (ScreenTransform-height handle) 2)
                                                            (Arith-div arith (cairo:cairo-text-extents-t-width extents) 2))))
               (cairo:cairo-rotate cr (Arith-div arith (Arith-sub arith pi) 2))
               (cairo:cairo-show-text cr title)
               (cairo:cairo-restore cr))))))

(defun draw-grid (cr handle n)
  (let* ((pwidth (Arith-mul arith (ScreenTransform-width handle) 0.6))
         (pheight (Arith-mul arith (ScreenTransform-height handle) 0.6))
         (woff (Arith-mul arith (ScreenTransform-width handle) 0.2))
         (hoff (Arith-mul arith (ScreenTransform-height handle) 0.2)))
    (cairo:cairo-set-source-rgb cr 0.6 0.6 0.6)
    (cairo:cairo-set-line-width cr 0.5)
    (mapcar (lambda (x)
              (apply 'cairo:cairo-move-to cr
                     (ScreenTransform-transform handle
                                                (Arith-add arith woff x)
                                                (Arith-div arith hoff 2)))
              (apply 'cairo:cairo-line-to cr
                     (ScreenTransform-transform handle
                                                (Arith-add arith woff x)
                                                (Arith-sub arith
                                                           (ScreenTransform-height handle)
                                                           (Arith-div arith hoff 2)))))
            (linspace 0 pwidth n arith))
    (mapcar (lambda (y)
              (apply 'cairo:cairo-move-to cr
                     (ScreenTransform-transform handle
                                                (Arith-div arith woff 2)
                                                (Arith-add arith hoff y)))
              (apply 'cairo:cairo-line-to cr
                     (ScreenTransform-transform handle
                                                (Arith-sub arith
                                                           (ScreenTransform-width handle)
                                                           (Arith-div arith woff 2))
                                                (Arith-add arith hoff y))))
            (linspace 0 pheight n arith))
    (cairo:cairo-stroke cr)))

(defun draw-ticks (cr handle pos min-val max-val)
  (let* ((woff (Arith-mul arith (ScreenTransform-width handle) 0.2))
         (hoff (Arith-mul arith (ScreenTransform-height handle) 0.2))
         (min-title (format nil "~0,3g" min-val))
         (max-title (format nil "~0,3g" max-val))
         (extents nil))
    (cairo:cairo-set-source-rgb cr 0.0 0.0 0.0)
    (cond ((equal 'x pos)
           (setf extents (cairo:cairo-text-extents cr min-title))
           (apply 'cairo:cairo-move-to cr
                  (ScreenTransform-transform handle
                                             (Arith-sub arith
                                                        woff
                                                        (Arith-div arith (cairo:cairo-text-extents-t-width extents) 2))
                                             (Arith-sub arith
                                                        (Arith-div arith hoff 2)
                                                        (cairo:cairo-text-extents-t-height extents))))
           (cairo:cairo-show-text cr min-title)
           (setf extents (cairo:cairo-text-extents cr max-title))
           (apply 'cairo:cairo-move-to cr
                  (ScreenTransform-transform handle
                                             (Arith-sub arith
                                                        (Arith-sub arith (ScreenTransform-width handle) woff)
                                                        (Arith-div arith (cairo:cairo-text-extents-t-width extents) 2))
                                             (Arith-sub arith
                                                        (Arith-div arith hoff 2)
                                                        (cairo:cairo-text-extents-t-height extents))))
           (cairo:cairo-show-text cr max-title))
          ((equal 'y pos)
           (setf extents (cairo:cairo-text-extents cr min-title))
           (apply 'cairo:cairo-move-to cr
                  (ScreenTransform-transform handle
                                             (Arith-sub arith
                                                        (Arith-div arith woff 2)
                                                        (cairo:cairo-text-extents-t-width extents))
                                             (Arith-sub arith
                                                        hoff
                                                        (Arith-div arith (cairo:cairo-text-extents-t-height extents) 2))))
           (cairo:cairo-show-text cr min-title)
           (setf extents (cairo:cairo-text-extents cr max-title))
           (apply 'cairo:cairo-move-to cr
                  (ScreenTransform-transform handle
                                             (Arith-sub arith
                                                        (Arith-div arith woff 2)
                                                        (cairo:cairo-text-extents-t-width extents))
                                             (Arith-sub arith
                                                        (Arith-sub arith (ScreenTransform-height handle) hoff)
                                                        (Arith-div arith (cairo:cairo-text-extents-t-height extents) 2))))
           (cairo:cairo-show-text cr max-title)))))

(defun draw-plot-line (cr handle x-list y-list &optional weight &key mapf)
  (let ((color (if (or (not mapf) (not weight)) '(0 0 1) (funcall mapf weight))))
    (apply 'cairo:cairo-set-source-rgb cr color)
    (cairo:cairo-set-line-width cr 0.5)
    (apply 'cairo:cairo-move-to cr (ScreenTransform-transform-graph handle
                                                                    (car x-list)
                                                                    (car y-list)))
    (mapcar (lambda (x y)
              (let ((tpoint (ScreenTransform-transform-graph handle x y)))
                (apply 'cairo:cairo-line-to cr tpoint)
                (apply 'cairo:cairo-move-to cr tpoint)))
            x-list y-list)
    (cairo:cairo-stroke cr)))

(defun draw-plot (cr width height xy-lists &key x-title y-title weights mapf)
  (let* ((x-range (let ((x-list (apply 'append (mapcar (lambda (xy) (car xy))
                                                       xy-lists))))
                    (list (apply 'min x-list) (apply 'max x-list))))
         (y-range (let ((y-list (apply 'append (mapcar (lambda (xy) (cadr xy))
                                                       xy-lists))))
                    (list (apply 'min y-list) (apply 'max y-list))))
         (x-len (Arith-sub arith (cadr x-range) (car x-range)))
         (y-len (Arith-sub arith (cadr y-range) (car y-range)))
         (w-offset (Arith-mul arith width 0.1))
         (h-offset (Arith-mul arith height 0.1))
         (pwidth (Arith-mul arith width 0.6))
         (pheight (Arith-mul arith height 0.6))
         (x-scale (Arith-div arith pwidth x-len))
         (y-scale (Arith-sub arith (Arith-div arith pheight y-len)))
         (handle (make-ScreenTransform :width width
                                       :height height
                                       :w-offset (Arith-mul arith 2 w-offset)
                                       :h-offset (Arith-sub arith height (Arith-mul arith 2 h-offset))
                                       :x-scale x-scale
                                       :y-scale y-scale
                                       :x-min (car x-range)
                                       :y-min (car y-range)))
         (weights (if (not weights) (if (not (cadr xy-lists)) '(0) (linspace 0 1 (length xy-lists) arith)) weights))
         (mapf (if (not mapf) 'default-map mapf)))
    ;;; Draw axis
    (draw-grid cr handle 11)
    (cairo:cairo-set-source-rgb cr 0.0 0.0 0.0)
    (cairo:cairo-set-line-width cr 0.5)
    (apply 'cairo:cairo-move-to cr (ScreenTransform-transform handle w-offset h-offset))
    (apply 'cairo:cairo-line-to cr (ScreenTransform-transform handle w-offset (Arith-sub arith height h-offset)))
    (apply 'cairo:cairo-move-to cr (ScreenTransform-transform handle w-offset h-offset))
    (apply 'cairo:cairo-line-to cr (ScreenTransform-transform handle (Arith-sub arith width w-offset) h-offset))
    (cairo:cairo-stroke cr)
    (mapcar (lambda (title pos) (draw-title cr handle title pos))
            (list x-title y-title) '(x y))
    (mapcar (lambda (pos v-min v-max) (draw-ticks cr handle pos v-min v-max))
            '(x y)
            (list (car x-range) (car y-range))
            (list (car (last x-range)) (car (last y-range))))
    ;;; Draw plots
    (mapcar (lambda (xy w) (funcall 'draw-plot-line cr handle (car xy) (cadr xy) w :mapf mapf)) xy-lists weights)))

(defun show-f-func (cr width height builder)
  (let ((f (construct-f-func builder))
        (x (linspace 0 (gtk:gtk-adjustment-get-value (gtk:gtk-builder-get-object builder "t_end_adj")) 100 arith)))
    (draw-plot cr width height (list (list x (mapcar (lambda (y) (funcall f y arith)) x)))
               :x-title "Время (с)"
               :y-title "Поток (Вт/см^2)")))

(defun show-main-graph (cr width height res)
  (let ((res (car res)))
    (if (not (null res)) (draw-plot cr width height (mapcar (lambda (y) (list (cadr res) y)) (caddr res))
                                    :x-title "Радиус (см)"
                                    :y-title "Температура (K)"))))

(defun find-borders-inner (lst val n)
  (cond ((not lst) n)
        ((<= (car lst) val) (find-borders-inner (cdr lst) val (Arith-add arith n 1)))
        (n)))

(defun find-borders (lst val) (find-borders-inner lst val -1))

(defun show-cut-t (cr width height lst)
  (let* ((builder (car lst))
         (res (car (cadr lst))))
    (if (not (null res))
        (let* ((r (gtk:gtk-adjustment-get-value (gtk:gtk-builder-get-object builder "r_scale_adj")))
               (pos (find-borders (cadr res) r))
               (temp-list (cond ((< pos 0) (mapcar (lambda (x) (car x)) (caddr res)))
                                ((= pos (Arith-sub arith (length (cadr res)) 1)) (mapcar (lambda (x) (car (last x))) (caddr res)))
                                ((let* ((border (nthcdr pos (cadr res)))
                                        (frac (Arith-div arith
                                                         (Arith-sub arith r (car border))
                                                         (Arith-sub arith (cadr border) (car border)))))
                                   (mapcar (lambda (x) (let ((tmp (nthcdr pos x)))
                                                         (Arith-add arith
                                                                    (Arith-mul arith (Arith-sub arith 1 frac) (car tmp))
                                                                    (Arith-mul arith frac (cadr tmp)))))
                                           (caddr res)))))))
          (draw-plot cr width height (list (list (car res) temp-list))
                     :x-title "Время (с)"
                     :y-title "Температура (K)")))))

(defun show-cut-r (cr width height lst)
  (let* ((builder (car lst))
         (res (car (cadr lst))))
    (if (not (null res))
        (let* ((temp (gtk:gtk-adjustment-get-value (gtk:gtk-builder-get-object builder "t_scale_adj")))
               (pos (find-borders (car res) temp))
               (temp-list (cond ((< pos 0) (car (caddr res)))
                                ((= pos (Arith-sub arith (length (car res)) 1)) (car (last (caddr res))))
                                ((let* ((border (nthcdr pos (car res)))
                                        (frac (Arith-div arith
                                                         (Arith-sub arith temp (car border))
                                                         (Arith-sub arith (cadr border) (car border))))
                                        (temp-list (nthcdr pos (caddr res))))
                                   (mapcar (lambda (x y)
                                             (Arith-add arith
                                                        (Arith-mul arith (Arith-sub arith 1 frac) x)
                                                        (Arith-mul arith frac y)))
                                           (car temp-list)
                                           (cadr temp-list)))))))
          (draw-plot cr width height (list (list (cadr res) temp-list))
                     :x-title "Радиус (см)"
                     :y-title "Температура (K)")))))

(defun show-mainwindow ()
  (gtk:within-main-loop
      (let ((builder (gtk:gtk-builder-new))
            (task-handle (make-TaskHandle))
            (res (list (list))))
        (gtk:gtk-builder-add-from-file builder "form.glade")
        (let ((window (gtk:gtk-builder-get-object builder "window")))
          (gobject:g-signal-connect window "destroy"
                                    (lambda (widget)
                                      (declare (ignore widget))
                                      (gtk:leave-gtk-main)))

          (gobject:g-signal-connect (gtk:gtk-builder-get-object builder "button_calculate") "clicked"
                                    (lambda (widget)
                                      (construct-task-handle task-handle builder)
                                      (setf (car res) (solve task-handle))
                                      (gtk:gtk-widget-queue-draw (gtk:gtk-builder-get-object builder "drawing_area_main"))
                                      (gtk:gtk-adjustment-set-lower (gtk:gtk-builder-get-object builder "r_scale_adj") (car (cadr (car res))))
                                      (gtk:gtk-adjustment-set-upper (gtk:gtk-builder-get-object builder "r_scale_adj") (car (last (cadr (car res)))))
                                      (gtk:gtk-adjustment-set-value (gtk:gtk-builder-get-object builder "r_scale_adj") (car (cadr (car res))))
                                      (gtk:gtk-adjustment-set-lower (gtk:gtk-builder-get-object builder "t_scale_adj") (car (car (car res))))
                                      (gtk:gtk-adjustment-set-upper (gtk:gtk-builder-get-object builder "t_scale_adj") (car (last (car (car res)))))
                                      (gtk:gtk-adjustment-set-value (gtk:gtk-builder-get-object builder "t_scale_adj") (car (car (car res))))
                                      (gtk:gtk-widget-queue-draw (gtk:gtk-builder-get-object builder "drawing_area_cut_t"))
                                      (gtk:gtk-widget-queue-draw (gtk:gtk-builder-get-object builder "drawing_area_cut_r"))))

          (gobject:g-signal-connect (gtk:gtk-builder-get-object builder "sclae_cut_t") "move_slider"
                                    (lambda (widget)
                                      (gtk:gtk-widget-queue-draw (gtk:gtk-builder-get-object builder "drawing_area_cut_t"))))

          (gobject:g-signal-connect (gtk:gtk-builder-get-object builder "scale_cut_r") "move_slider"
                                    (lambda (widget)
                                      (gtk:gtk-widget-queue-draw (gtk:gtk-builder-get-object builder "drawing_area_cut_r"))))

          (gobject:g-signal-connect (gtk:gtk-builder-get-object builder "drawing_area_flux") "draw"
                                    (get-draw-wrap 'show-f-func builder))
          (gobject:g-signal-connect (gtk:gtk-builder-get-object builder "drawing_area_main") "draw"
                                    (get-draw-wrap 'show-main-graph res))
          (gobject:g-signal-connect (gtk:gtk-builder-get-object builder "drawing_area_cut_t") "draw"
                                    (get-draw-wrap 'show-cut-t (list builder res)))
          (gobject:g-signal-connect (gtk:gtk-builder-get-object builder "drawing_area_cut_r") "draw"
                                    (get-draw-wrap 'show-cut-r (list builder res)))
          (mapcar (lambda (obj)
                    (let ((area (gtk:gtk-builder-get-object builder "drawing_area_flux")))
                      (gobject:g-signal-connect obj "value-changed"
                                                (lambda (widget)
                                                  (gtk:gtk-widget-queue-draw area)))))
                  (list (gtk:gtk-builder-get-object builder "spin_button_F_max")
                        (gtk:gtk-builder-get-object builder "spin_button_t_max")
                        (gtk:gtk-builder-get-object builder "spin_button_t_end")
                        (gtk:gtk-builder-get-object builder "spin_button_v")))
          (gtk:gtk-widget-show-all window)))))

