(defclass matrix ()
  ((rows :accessor row-list
         :initarg :row-list
         :initform nil)))

;; check if the input is a matrix
(defun matrixp (m)
  (eql (type-of m) 'matrix))

(defmacro make-generic (x y)
  `(defgeneric ,x ,y 
     (:documentation "blah")))

(make-generic col-dim (a))
(make-generic row-dim (a))
(make-generic nrow (a b))
(make-generic ncol (a b))
(make-generic dot (a b))
(make-generic getij (a b c))
(make-generic setij (a b c d))
(make-generic m* (a b))
(make-generic mexp (a b))

(defmethod col-dim ((m matrix))
  (length (car (row-list m))))

(defmethod row-dim ((m matrix))
  (length (row-list m)))

(defmethod nrow ((n number) (m matrix))
  (if (<= n (row-dim m))
      (nth (1- n) (row-list m))
      (error "row dimension out of bounds")))

(defmethod ncol ((n number) (m matrix))
  (if (<= n (col-dim m))
      (loop for x in (row-list m)
            collect (nth (1- n) x))
      (error "col dimension out of bounds")))

(defmethod dot ((a list) (b list))
  (if (eql (length a) (length b))
      (apply '+ (loop for x in a
                      for y in b
                      collect (* x y)))
      (error "lists of unequal length")))

(defun new-matrix (x)
  (defun okay-for-new (y)
    (let ((is-okay t))
      (loop for z in y
            do (if (not (and (listp z)
                             (eql (length (car y))
                                  (length z))))
                   (setf is-okay nil)))
      is-okay))
  (if (okay-for-new x)
      (let ((m nil))
        (setf m (make-instance 'matrix
                               :row-list x))
        m)
      (error "input poorly formatted")))

(defmethod getij ((i number) (j number) (m matrix))
  (if (or (> i (row-dim m))
          (> j (col-dim m))
          (< i 1)
          (< j 1))
      (error "requested entry out of bounds")
      (nth (1- j) (nrow i m))))

(defmethod setij ((i number) (j number) (x number) (m matrix))
  (if (or (> i (row-dim m))
          (> j (col-dim m))
          (< i 1)
          (< j 1))
      (error "requested entry out of bounds")
      (setf (nth (1- j) (nrow i m)) x)))

(defmethod m* ((m matrix) (n matrix))
  (defun okay-for-mult (m n)
    (eql (col-dim m) (row-dim n)))
  (if (okay-for-mult m n)
      (new-matrix 
        (loop for i from 1 to (row-dim m)
              collect (loop for j from 1 to (col-dim n)
                            collect (dot (nrow i m) (ncol j n)))))
      (error "matrices cannot be multiplied")))

(defmethod mexp ((m matrix) (n integer))
  (defun mexp-iter (matrix-storage counter)
    (if (<= counter 1)
        matrix-storage
        (mexp-iter (m* matrix-storage m) (- counter 1))))
  (mexp-iter m n))
      
(defun matrix-from-ij-function (rows cols f)
  (new-matrix
    (loop for i from 1 to rows
          collect (loop for j from 1 to cols
                        collect (funcall f i j)))))

(defun zero-matrix (dimension)
  (matrix-from-ij-function dimension dimension
                           (lambda (x y) (* 0 x y))))

(defun identity-matrix (dimension)
  (matrix-from-ij-function dimension dimension
                           (lambda (x y)
                             (if (eql x y) 1 0))))

(defmethod display ((m matrix))
  (loop for x in (row-list m)
        do (prin1 (format nil "~A~%" x))))

(defmethod display2 ((m matrix))
  (loop for x in (row-list m)
        do (progn (loop for y in x
                        do (prin1 (format nil "~A " y)))
                  (prin1 (format nil "~%")))))
