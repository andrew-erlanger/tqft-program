;;; Continued fraction calculator

(defun invert (x) (/ 1 x))

(defun calc-cont-frac-backwards (z)
  (let ((z-ceiling (ceiling z)))
    (if (eql (- z-ceiling z) 0)
        (cons z nil)
        (cons z-ceiling (calc-cont-frac-backwards (invert (- z-ceiling
                                                             z)))))))

(defun calc-cont-frac (z)
  (reverse (calc-cont-frac-backwards z)))

;;(defun continued-fraction-list->rational-number (x)
;;  (if (let ((okay-to-proceed t))
	
