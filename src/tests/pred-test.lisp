;;; -*- Package: C -*-
(in-package 'c)

(defun sequence-test (x)
  (typep x 'sequence))

(defun bit-test (x)
  (typep x 'bit))

