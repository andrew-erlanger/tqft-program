(defparameter t-matrix
  (new-matrix (list (list 1 1) (list 0 1))))
(defparameter s-matrix
  (new-matrix (list (list 0 -1) (list 1 0))))

(defun s-induced-function (i j k)
  (* (sqrt (/ 2 (+ k 2)))
     (sin (/ (* i j pi) (+ k 2)))))

(defun s-induced-matrix (k)
  (matrix-from-ij-function (+ k 1)
                           (+ k 1)
                           (lambda (i j)
                             (s-induced-function i j k))))

(defun delta (i j)
  (if (eql i j) 1 0))

(defun square (x) (* x x))

(defun conformal-weight (i k)
  (/ (- (square i) 1) 
     (* 4 (+ k 2))))

(defun central-charge (k)
  (/ (* 3 k) (+ k 2)))

(defun t-induced-function (i j k)
  (if (eql (delta i j) 0)
      0
      (exp (* 2 pi #c(0 1)
              (- (conformal-weight i k)
                 (/ (central-charge k) 24))))))

;; level k
(defun t-induced-matrix (k)
  (matrix-from-ij-function (+ k 1)
                           (+ k 1)
                           (lambda (i j)
                             (t-induced-function i j k))))

;; integer p, level k
(defmethod m-induced ((p integer) (k integer))
  (m* (mexp (t-induced-matrix k) p)
      (s-induced-matrix k)))

;; rational number p, level k
(defmethod m-induced ((p ratio) (k integer))
  (let ((m (identity-matrix (+ k 1))))
    (loop for x in (calc-cont-frac p)
          do (setf m (m* (m-induced x k) m)))
    m))

(defmethod lens-invariant ((p number) (k integer))
  (getij 1 1 (m-induced (invert p) k)))

  
