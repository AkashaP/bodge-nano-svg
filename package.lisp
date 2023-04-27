(defpackage :libnanosvg
  (:use :cl :cffi)
  (:export
   :svg-draw
   :svg->nanovg-paint

   :dictify
   :paint
   
   :image-width
   :image-height)) 
