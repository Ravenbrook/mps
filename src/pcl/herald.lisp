(in-package "PCL")

#+(or loadable-pcl bootable-pcl)
(progn
  (defvar *pcl-system-date* "$Date: 2003/05/04 13:11:21 $")
  (setf (getf *herald-items* :pcl)
	`("    CLOS based on Gerd's PCL " ,(subseq *pcl-system-date* 7 26))))

