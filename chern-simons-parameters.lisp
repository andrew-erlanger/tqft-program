(defun delta (i j)
  (if (eql i j) 1 0))

(defun square (x) (* x x))

(defun conformal-weight (i k)
  (/ (- (square i) 1) 
     (* 4 (+ k 2))))

(defun central-charge (k)
  (/ (* 3 k) (+ k 2)))
