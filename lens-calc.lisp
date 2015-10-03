(defun calc-lens-table (start-level end-level p q)
  (let ((my-table (new-table (format nil
                                     "./data/lens_~A_~A_from_~A_to_~A.txt"
                                     p q
                                     start-level
                                     end-level)))
        (fraction (/ p q)))
    (open-table my-table)
    (loop for level from start-level to end-level
          do (write-to-table my-table
                             (print-complex-at-level level
                                (clean-complex
                                   (lens-invariant fraction level)))))
    (close-table my-table)))


(defun calc-lens-table-range (start-level end-level
                              firstp lastp
                              firstq lastq)
  (loop for p from firstp to lastp
        do (loop for q from firstq to lastq
                 do (calc-lens-table start-level end-level p q))))

(defmethod clean-complex ((z complex))
  (defun kill-small (x)
    (if (> 1e-5 (abs x)) 0 x))
  (let ((x (realpart z)) (y (imagpart z)))
        (setf x (kill-small x))
        (setf y (kill-small y))
        (complex x y)))

