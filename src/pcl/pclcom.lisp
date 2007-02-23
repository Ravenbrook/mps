;;I think this isn't used; instead, "target:tools/pclcom.lisp" is used.
(setq ext:*gc-verbose* nil)
(setq c:*suppress-values-declaration* t)
(setf (search-list "pcl:") '("build:pcl/"))
(load "pcl:defsys")
(in-package "PCL")
(import 'kernel:funcallable-instance-p)
(with-compilation-unit ()
  (pcl::compile-pcl))
