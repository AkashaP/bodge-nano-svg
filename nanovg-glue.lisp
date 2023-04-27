(in-package :libnanosvg)

(defun get-line-crossing (p0x p0y p1x p1y p2x p2y p3x p3y)
  "Line intersection algorithm.

If it doesn't intersect in the range provided or is collinear it returns NaN."
  ;; other line intersection algorithms extrapolate the lines and don't work collinearly, beware! 
  (declare (ignorable p1y p3y))
  (let ((bx (- p2x p0x))
        (by (- p2y p0y))
        (dx (- p1x p0x))
        (dy (- p1x p0y))
        (ex (- p3x p2x))
        (ey (- p3x p2y)))
    (let ((m (- (* dx ey) (* dy ex))))
      (if (< (abs m) 1e-6)
          float-features:single-float-nan
          (/ (- (* dx by) (* dy bx)) m)))))

(defun NSVG-color->NVG-color (color) 
  (with-foreign-object (claw-result '(claw-utils:claw-pointer %nanovg::color))
    (%nanovg:rgba claw-result
                  (logand (ash color -0) #xff)
                  (logand (ash color -8) #xff)
                  (logand (ash color -16) #xff)
                  (logand (ash color -24) #xff))))

(defun NSVG-paint->NVG-paint (context paint) 
  (with-foreign-objects ((claw-result '(claw-utils:claw-pointer %nanovg::color)))
    (with-slots ((type libnanosvg::type)
                 (g libnanosvg::color-gradient)) paint
      (assert (or (eq type :nsvg-paint-linear-gradient)
                  (eq type :nsvg-paint-radial-gradient)))
      (with-slots ((stops libnanosvg::stops)
                   (nstops libnanosvg::nstops)) g
        (assert (<= 1 nstops))
        (let ((icol (NSVG-color->NVG-color (gradient-stop-color (elt stops 0))))
              (ocol (NSVG-color->NVG-color (gradient-stop-color (elt stops (- nstops 1))))))

          (with-foreign-objects ((sx :float)
                                 (sy :float)
                                 (ex :float)
                                 (ey :float)
                                 (inverse :float 6))
            
            (%nanovg:transform-point sx
                                     sy
                                     inverse
                                     0.0 0.0)
            ;; NOTE Is is always 0->1?
            (%nanovg:transform-point sx
                                     sy
                                     inverse
                                     0.0 1.0)
            
            (case type
              (:nsvg-paint-linear-gradient
               (%nanovg:linear-gradient claw-result context (mem-ref sx :float) (mem-ref sy :float) (mem-ref ex :float) (mem-ref ey :float) icol ocol))
              (:nsvg-paint-radial-gradient
               (%nanovg:radial-gradient claw-result context (mem-ref sx :float) (mem-ref sy :float) (mem-ref ex :float) (mem-ref ey :float) icol ocol)))))))))

(defvar *screen-height* 600)

(defun svg-draw (context image &key (screen-height *screen-height*))
  "Render SVG using Nanovg immediately. Inverted by screen-height."
  (declare (type libnanosvg::image image))
  (with-slots ((shapes libnanosvg::shapes)) image
    (loop with iy = screen-height
          for shape across shapes
          for shape-index from 0
          if (eq 1 (logand (libnanosvg::shape-flags shape) 1))
            do (with-slots
                     ((paths libnanosvg::paths)
                      (sx1 libnanosvg::x1) 
                      (sy1 libnanosvg::y1) 
                      (sx2 libnanosvg::x2) 
                      (sy2 libnanosvg::y2)
                      (opacity libnanosvg::opacity)
                      (fill libnanosvg::fill)
                      (stroke libnanosvg::stroke)                    
                      (stroke-width libnanosvg::stroke-width)
                      (line-cap libnanosvg::stroke-line-cap)
                      (line-join libnanosvg::stroke-line-join)) shape
                 
                 ;; (%nanovg:save context) 
                 (%nanovg:global-alpha context opacity)
                 (%nanovg:begin-path context)
                 (loop for path across paths
                       do (with-slots
                                ((pts libnanosvg::pts)
                                 (closed libnanosvg::pts)) path
                            (%nanovg:move-to context (elt pts 0) (- iy (elt pts 1)))
                            (loop for i from 1 by 3 below (/ (length pts) 2)
                                  do (%nanovg:bezier-to context
                                                        (elt pts (+ 0 (* 2 i)))
                                                        (- iy (elt pts (+ 1 (* 2 i))))
                                                        (elt pts (+ 2 (* 2 i)))
                                                        (- iy (elt pts (+ 3 (* 2 i))))
                                                        (elt pts (+ 4 (* 2 i)))
                                                        (- iy (elt pts (+ 5 (* 2 i))))))

                            (if closed
                                (%nanovg:close-path context))

                            ;; Calculate hole/solid by even-odd rule
                            (let ((crossings 0))
                              (loop for path2 across paths
                                    do (with-slots
                                             ((pts2 libnanosvg::pts)
                                              (bounds libnanosvg::bounds)) path2
                                         (if (and (not (eq path2 path))
                                                  (< (/ (length pts2) 2) 4))
                                             (loop for i from 1 by 3 below (+ 3 (/ (length pts2) 2))
                                                   for full = (< i (/ (length pts2) 2))
                                                   for crossing = (get-line-crossing
                                                                   ;;p0
                                                                   (elt pts 0)
                                                                   (- iy (elt pts 1))
                                                                   ;; p1
                                                                   (- (elt bounds 0) 1.0)
                                                                   (- iy (elt bounds 1) 1.0)
                                                                   ;; p2 
                                                                   (elt pts (- (* 2 i) 2))
                                                                   (- iy (elt pts (- (* 2 i) 1)))
                                                                   ;; p3
                                                                   (elt pts (if full 0 (+ 4 (* 2 i))))
                                                                   (- iy (elt pts (if full 0 (+ 5 (* 2 i))))))
                                                   for crossing2 = (get-line-crossing
                                                                    ;; p2
                                                                    (elt pts (- (* 2 i) 2))
                                                                    (- iy (elt pts (- (* 2 i) 1)))
                                                                    ;; p3
                                                                    (elt pts (if full 0 (+ 4 (* 2 i))))
                                                                    (- iy (elt pts (if full 0 (+ 5 (* 2 i)))))
                                                                    ;; p0
                                                                    (elt pts 0)
                                                                    (- iy (elt pts 1))
                                                                    ;; p1
                                                                    (- (elt bounds 0) 1.0)
                                                                    (- iy (elt bounds 1) 1.0))
                                                   if (and (<= 0 crossing)
                                                           (<= 0 crossing2)
                                                           (< crossing 1))
                                                     do (incf crossings)))))
                              
                              (if (= 0 (mod crossings 2))
                                  (%nanovg:path-winding context ;:solid
                                                        1)
                                  (%nanovg:path-winding context ;:hole
                                                        2)))

                            ;; (let ((area 0.0))
                            ;;   (setq area
                            ;;         (loop
                            ;;           with p0x = (elt pts 0)
                            ;;           with p0y = (elt pts 1)
                            ;;           for i from 1 by 3 below (/ (length pts) 2)
                            ;;           for p1x = (if (< i (/ (length pts) 2))
                            ;;                         (elt pts (+ (* 2 i) 4))
                            ;;                         0) 
                            ;;           for p1y = (if (< i (/ (length pts) 2))
                            ;;                         (elt pts (+ (* 2 i) 5))
                            ;;                         0)
                            ;;           sum (* .5 (- p1x p0x) (+ p1y p0y))
                            ;;           do (setq p0x p1x
                            ;;                    p0y p1y)))
                            ;;   (if (< area 0.0)
                            ;;       (%nanovg:path-winding context 1)
                            ;;       (%nanovg:path-winding context 2)))
                            ))

                 (when fill
                   (with-slots
                         ((type libnanosvg::type)
                          (color-or-gradient libnanosvg::color-gradient)) fill
                     (ecase type
                       (:nsvg-paint-color 
                        (%nanovg:fill-color context (NSVG-color->NVG-color color-or-gradient)))
                       ((or :nsvg-paint-linear-gradient
                            :nsvg-paint-radial-gradient)
                        (%nanovg:fill-paint context (NSVG-paint->NVG-paint context fill)))
                       (:nsvg-paint-none))
                     (unless (eq :nsvg-paint-none type)
                       (%nanovg:fill context))))

                 ;; stroke
                 (when stroke
                   (with-slots
                         ((type libnanosvg::type)
                          (color-or-gradient libnanosvg::color-gradient)) stroke 
                     (unless (eq :nsvg-paint-none type)
                       (%nanovg:stroke-width context stroke-width)
                       (%nanovg:line-cap context line-cap)
                       (%nanovg:line-join context line-join))
                     (ecase type
                       (:nsvg-paint-color 
                        (%nanovg:stroke-color context (NSVG-color->NVG-color color-or-gradient)))
                       ((or :nsvg-paint-linear-gradient
                            :nsvg-paint-radial-gradient)
                        (%nanovg:stroke-paint context (NSVG-paint->NVG-paint context color-or-gradient)))
                       (:nsvg-paint-none))
                     (unless (eq :nsvg-paint-none type) 
                       (%nanovg:stroke context))))))))

;; (defvar *svg-rasteriser* (nanosvg:create-rasterizer))
;; (defvar *svg-rasteriser* (foreign-alloc :pointer :count 1 :initial-contents
;;                                         (list (nanosvg:create-rasterizer))))
;; (defvar *svg-rasteriser* (cffi:foreign-alloc 'nanosvg::rasterizer))

(defstruct svg-nanovg-paint image-id paint mem)

(defun paint (nvg-paint)
  (svg-nanovg-paint-paint nvg-paint))

;; (defvar *debug-msgs* nil)

(defun svg->nanovg-paint (context foreign-svg
                          &key (scale 1.0) (ox 0.0) (oy 0.0) (repeatx t) (repeaty t)
                            (w (cffi:foreign-slot-value foreign-svg '(:struct nanosvg:image) 'nanosvg:width))
                            (h (cffi:foreign-slot-value foreign-svg '(:struct nanosvg:image) 'nanosvg:height)))
  "Render SVG to NanoVG paint
FOREIGN-SVG is result of nanosvg:parse. It is a foreign structure."
  (let (id
        fimg
        (w (floor (* scale w)))
        (h (floor (* scale h)))
        (res (make-svg-nanovg-paint)))
    (unwind-protect
         (progn
           (setq fimg (cffi-sys:%foreign-alloc (* 4 w h)))
           
           ;; (nanosvg:rasterize *svg-rasteriser* foreign-svg ox oy scale fimg w h (* w 4))
           ;; (nanosvg:rasterize (mem-ref *svg-rasteriser* :pointer) foreign-svg ox oy scale fimg w h (* w 4))
           (nanosvg:rasterize ;*svg-rasteriser*
            (nanosvg:create-rasterizer) foreign-svg ox oy scale fimg w h (* w 4))

           ;; (when *debug-msgs*
           ;;   (format t "~a size: ~a by ~a (~a)" id w h (* w h)))
           
           (setq id (%nanovg:create-image-rgba context
                                               w h
                                               (cffi:foreign-bitfield-value '%nanovg:image-flags
                                                                            (nconc (list :generate-mipmaps)
                                                                                   (when repeatx
                                                                                     (list :repeatx))
                                                                                   (when repeaty
                                                                                     (list :repeaty))
                                                                                   (list :flipy)
                                                                                   ;; (list :nearest)
                                                                                   ))
                                               fimg)))
      (cffi:foreign-free fimg))
    
    (with-slots (image-id paint mem) res
      (setq image-id id
            mem (cffi:foreign-alloc '(:struct %nanovg:paint))
            paint (%nanovg:image-pattern mem context
                                         0.0 0.0 (float w) (float h)
                                         0.0 image-id 1.0))
      res)))
