(asdf:defsystem :bodge-nano-svg
  :description ""
  :author "Akasha Peppermint"
  :mailto ""
  :license "MIT"
  :depends-on (:cffi :bodge-nanovg :nanosvg-blob)
  :components ((:file "package")
               (:file "structures")
               (:file "nanovg-glue")) 
  :serial t)
