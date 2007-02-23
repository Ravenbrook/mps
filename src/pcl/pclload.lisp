(in-package "PCL")
(rename-package "PCL" "PCL" '("OLD-PCL"))
(import 'kernel:funcallable-instance-p)
(load "target:pcl/defsys")
(load-pcl)
(rename-package "PCL" "PCL" '())


