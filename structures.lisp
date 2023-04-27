(in-package :libnanosvg)

;; NOTE
;; converting foreign types to lisp types gives the advantage of garbage collection, inspector, etc.
;; leaving them as foreign types allows manual memory management and needed for other libnanosvg calls.

(deftype paint-type (&optional type) 
  `(member ,type
           '(:nsvg-paint-none
             :nsvg-paint-color
             :nsvg-paint-linear-gradient
             :nsvg-paint-radial-gradient)))

(deftype spread-type (&optional type) 
  `(member ,type
           '(:nsvg-spread-pad 
             :nsvg-spread-reflect
             :nsvg-spread-repeat)))

(deftype line-join (&optional type) 
  `(member ,type
           '(:nsvg-join-miter
             :nsvg-join-round
             :nsvg-join-bevel)))

(deftype line-cap (&optional type) 
  `(member ,type
           '(:nsvg-cap-butt
             :nsvg-cap-round
             :nsvg-cap-square)))

(deftype fillrule (&optional type) 
  `(member ,type
           '(:nsvg-fillrule-nonzero
             :nsvg-fillrule-evenodd)))

(deftype flags (&optional type) 
  `(eq ,type :nsvg-flags-visible))

(defstruct gradient-stop
  (color 0 :type integer) ; :unsigned-int
  (offset 0.0 :type float))

(defstruct gradient
  (xform (cffi:null-pointer) :type cffi:foreign-pointer)
  (spread 0 :type integer)
  (fx 0.0 :type float)
  (fy 0.0 :type float)
  (nstops 0 :type integer)
  (stops nil :type array))

(deftype color-gradient (&optional type) 
  `(or (typep ,type integer)
       (typep ,type gradient)))

(defstruct paint
  (type nil)
  (color-gradient nil))

(defstruct path
  (pts nil :type array)
  ;; (npts :int)
  (closed nil :type boolean)
  (bounds nil :type array)
  ;; below unused
  (x1 0.0 :type float)
  (y1 0.0 :type float)
  (x2 0.0 :type float)
  (y2 0.0 :type float)
  ;; (next nil :type path) ; we are using arrays
  )

(defstruct shape
  (id "" :type string)
  (fill nil)
  (stroke nil)
  (opacity 0.0 :type float)
  (stroke-width 0.0 :type float)
  ;; not supported
  ;; (stroke-dash-offset 0.0 :type float)
  ;; (stroke-dash-array 0.0 :type float)
  ;; (stroke-dash-count 0.0 :type integer)
  (stroke-line-join 0 :type integer)
  (stroke-line-cap 0 :type integer)
  (miter-limit 0.0 :type float)
  (fill-rule 0 :type integer)
  (flags 0 :type integer)
  (x1 0.0 :type float)
  (y1 0.0 :type float)
  (x2 0.0 :type float)
  (y2 0.0 :type float)
  (paths nil :type array)
  (bounds nil :type array)
  ;; (next nil :type array) ; we are using arrays
  )

(defstruct image
  (width 0.0 :type float)
  (height 0.0 :type float)
  (shapes nil :type array)
  dict)

(defun foreign-linkedlist->array (linked-list foreign-type foreign-next-slot)
  (coerce (loop with ll = linked-list 
                while (not (null-pointer-p ll))
                collect ll
                do (setq ll (foreign-slot-value ll foreign-type foreign-next-slot)))
          'vector))

;; We can store a dict of shapes for easy lookup by id in shape
;; since ID is meant to be unique...
(defun dictify (image)
  (with-slots (dict shapes) image
    (setq dict (make-hash-table))
    (loop for shape across shapes
          do (with-slots (id) shape
               (when (stringp id)
                 (setf (gethash (intern (string-upcase id) :keyword) dict)
                       shape))))))

;; libnanosvg stores shapes, paths, etc. in linkedlists
;; we convert them to arrays to make them easier to see in the inspector if something was wrong
;; amongst other reasons

(defun copy-symbol-into-package (symbol package)
  (let ((sym (find-symbol (symbol-name symbol) package)))
    (if (not sym)
        (progn (import symbol package)
               (find-symbol (symbol-name symbol) package))
        sym)))

(define-translation-method (o nanosvg::gradientstop :from)
  (let ((plist (call-next-method)))
    ;; note color is a uint
    (make-gradient-stop :color (getf plist 'nanosvg::color)
                        :offset (getf plist 'nanosvg::offset))))

(define-translation-method (o nanosvg::gradient :from)
  (let ((plist (call-next-method)))
    (make-gradient :xform (getf plist 'nanosvg::xform)
                   :spread (getf plist 'nanosvg::spread) 
                   :fx (getf plist 'nanosvg::fx)
                   :fy (getf plist 'nanosvg::fy)
                   :nstops (getf plist 'nanosvg::nstops)
                   ;; NOTE this feels like an array but it's size 1 for some reason
                   :stops (convert-from-foreign (mem-ref (getf plist 'nanosvg::stops) 'nanosvg::gradientstop) `(:array (:struct nanosvg::gradientstop) ,(getf plist 'nanosvg::nstops))))))

(define-translation-method (o nanosvg::paint :from)
  (let* ((plist (call-next-method))
         (type (getf plist 'nanosvg::type))
         (type (etypecase type
                 (number (cond
                           ((eq 0 type) :nsvg-paint-none)
                           ((eq 1 type) :nsvg-paint-color)
                           ((eq 2 type) :nsvg-paint-linear-gradient)
                           ((eq 3 type) :nsvg-paint-radial-gradient)))
                 (cffi:foreign-pointer (cffi:convert-from-foreign type '(:struct nanosvg::gradient))))))
    (make-paint :type type
                :color-gradient
                (if (or (eq :nsvg-paint-linear-gradient type)
                        (eq :nsvg-paint-radial-gradient type))
                    (cffi:convert-from-foreign (getf (getf plist 'nanosvg::color-gradient) 'nanosvg::gradient) '(:struct nanosvg::gradient))
                    (getf (getf plist 'nanosvg::color-gradient) 'nanosvg::color)))))

(define-translation-method (o nanosvg::image :from)
  (let ((plist (call-next-method)))
    (make-image :width (getf plist 'nanosvg::width)
                :height (getf plist 'nanosvg::height)
                :shapes (map 'vector (lambda (z) (cffi:convert-from-foreign z '(:struct nanosvg::shape)))
                             (foreign-linkedlist->array (getf plist 'nanosvg::shapes) 'nanosvg::shape 'nanosvg::next)))))

(define-translation-method (o nanosvg::shape :from)
  (let* ((plist (call-next-method)))
    (make-shape
     :id (alexandria:if-let ((string (ignore-errors (foreign-string-to-lisp (getf plist 'nanosvg::id)))))
           string
           "anonymous")
     :fill (alexandria:if-let ((fill (getf plist 'nanosvg::fill))) (convert-from-foreign fill '(:struct nanosvg::paint))) 
     :stroke (alexandria:if-let ((stroke (getf plist 'nanosvg::stroke))) (convert-from-foreign stroke '(:struct nanosvg::paint))) 
     :opacity (getf plist 'nanosvg::opacity 0.0) 
     :stroke-width (getf plist 'nanosvg::stroke-width 0.0)  
     :stroke-line-join (getf plist 'nanosvg::stroke-line-join 0)   
     :stroke-line-cap (getf plist 'nanosvg::stroke-line-cap 0)   
     :miter-limit (getf plist 'nanosvg::miter-limit 0.0) 
     :fill-rule (getf plist 'nanosvg::fill-rule 0)    
     :flags (getf plist 'nanosvg::flags 0)    
     :bounds (convert-from-foreign (getf plist 'nanosvg::bounds) '(:array :float 4))
     :x1 (mem-ref (getf plist 'nanosvg::bounds) :float 0) 
     :y1 (mem-ref (getf plist 'nanosvg::bounds) :float 4)  
     :x2 (mem-ref (getf plist 'nanosvg::bounds) :float 8) 
     :y2 (mem-ref (getf plist 'nanosvg::bounds) :float 12)
     :paths (map 'vector (lambda (z) (cffi:convert-from-foreign z '(:struct nanosvg::path)))
                 (foreign-linkedlist->array (getf plist 'nanosvg::paths) 'nanosvg::path 'nanosvg::next)))))

(define-translation-method (o nanosvg::path :from)
  (let* ((plist (call-next-method)))
    (make-path
     :pts (coerce (cffi:convert-from-foreign (getf plist 'nanosvg::pts) `(:array :float ,(* 2 (getf plist 'nanosvg::npts)))) 'vector) 
     :closed (eq 1 (getf plist 'nanosvg::closed)) 
     :bounds (coerce (cffi:convert-from-foreign (getf plist 'nanosvg::bounds) `(:array :float 4)) 'vector))))
