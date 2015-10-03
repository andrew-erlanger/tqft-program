(defclass data-table ()
  ((filename :accessor filename
             :initarg :name
             :initform "")
   (output-stream :accessor out-str
                  :initarg :os
                  :initform '())))

(defun new-table (name-string)
  (make-instance 'data-table
                 :name (make-pathname :name name-string)
                 :os '()))

(defmethod open-table ((x data-table))
  (setf (out-str x) (open (filename x) :direction :output
                                       :if-exists :supersede)))

(defmethod close-table ((x data-table))
  (close (out-str x)))

(defmethod write-to-table ((x data-table) (y string))
  (format (out-str x) y))

(defmethod write-function ((x data-table))
  (lambda (y) (write-table x y)))

(defun pair (x y)
  (format '() "~$    ~$~%" x y))

(defmethod print-complex-at-level ((k integer) (z number))
  (if (equalp (imagpart z) 0)
      (format '() "~$      ~5$~%" k (realpart z))
      (format '() "~$      ~5$ + ~5$i~%" k (realpart z) (imagpart z))))
