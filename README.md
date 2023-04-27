# bodge-nano-svg
Nanosvg bindings. Load SVG files and integrate with bodge-nanovg.

A bit raw (you may need to free foreign structures manually) but here are a few 

# key functions:

## svg->nanovg-paint
Renders an SVG to an in-memory image. This is able to be drawn with bodge-nanovg as a fill-paint
i.e 
```lisp
(%nanovg:fill-paint bodge-canvas::*canvas-handle* (svg->nanovg-paint svg))
(ge:fill-path)
```
see nanovg docs on how to draw shapes.

## svg-draw
Renders an SVG immediately. Translates SVG directly into bodge-nanovg draw calls.

 

PRs always welcome. Here are some other miscellaneous code:
```lisp
(defun parse-svg (svg) 
  (let (lisp-structure foreign-structure (str1 (cffi:foreign-alloc :char :count (length svg))))
    (cffi:lisp-string-to-foreign svg str1 (length svg))
    (cffi:with-foreign-string (str2 "px")
      (setq foreign-structure (nanosvg:parse str1
                                             str2
                                             96.0)
            lisp-structure (cffi:convert-from-foreign foreign-structure '(:struct nanosvg::image))))
    (values lisp-structure foreign-structure)))

;; in your REPL
(parse-svg "<svg></svg>") ; First result is LISP data, second result is foreign data
```

If you wish to maintain a registry of SVGs, here's an example:

```lisp
(defstruct svg
  data
  pointer
  paint
  width
  height)
(defvar *svgs* (make-hash-table)) ; keyword to structs
(defun refresh-svgs ()
  ;; create structs
  (loop for id being the hash-keys of *svgs* 
          using (hash-value svg)
        do (multiple-value-bind (lisp-data foreign-data) (parse-svg (gamekit:resource-by-id id))
             (setf (gethash id *svgs*)
                   (make-svg
                    :data lisp-data                             
                    ;; !!!! REMEMBER TO FREE THIS !!!!
                    :pointer foreign-data))))

  ;; create ID dicts
  (loop for id being the hash-keys of *svgs* 
          using (hash-value svg)
        do (with-slots (data) svg
             (libnanosvg:dictify data)))

  ;; create bounds
  (loop for id being the hash-keys of *svgs* 
          using (hash-value svg)
        do (with-slots (data width height) svg
             (with-slots ((dict libnanosvg::dict)
                          (shapes libnanosvg::shapes)) data
               (if-let ((d (gethash :bounds dict)))
                 (with-slots ((x1 libnanosvg::x1) (x2 libnanosvg::x2) (y1 libnanosvg::y1) (y2 libnanosvg::y2))
                     d
                   (let ((w (math:diff x1 x2))
                         (h (math:diff y1 y2)))
                     (setq width w
                           height h)))
                 (loop for s across shapes
                       minimizing (slot-value s 'libnanosvg::x1) into x1 
                       minimizing (slot-value s 'libnanosvg::y1) into y1 
                       maximizing (slot-value s 'libnanosvg::x2) into x2 
                       maximizing (slot-value s 'libnanosvg::y2) into y2
                       finally (setq width (math:diff x1 x2)
                                     height (math:diff y1 y2))))))))
```

Relevant links:
- https://github.com/memononen/nanosvg the main SVG parsing library
- https://github.com/rezrov/libnanosvg ... as a shared library
- https://github.com/AkashaP/nanosvg-blob ... the shared library (uses bodge-blobs-support)
- https://github.com/AkashaP/bodge-nano-svg ... bindings for such. as well as intergration with:
- https://github.com/memononen/nanovg the vector graphics rendering library
- https://github.com/borodust/bodge-nanovg ... bindings for so
