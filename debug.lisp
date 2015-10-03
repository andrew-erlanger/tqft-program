(defparameter t1 (t-induced-matrix 2))

(defparameter t2 (m* t1 t1))

(defparameter t3 (m* t2 t1))

(defparameter s1 (s-induced-matrix 2))

(defparameter m1 (m* t1 s1))

(defparameter m2 (m* m1 t2))

(defparameter m3 (m* m2 s1))

(defparameter m4 (m* m3 t3))

(defparameter m5 (m* m4 s1))

(getij 1 1 m5)
