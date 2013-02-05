
(in-package :cl-user)

(asdf:defsystem :smithzv.destructuring-bind-iterate
  :author "Zach Kost-Smith"
  :license "BSD"
  :components ((:file "for-bind"))
  :depends-on (:iterate))
