(defmethod sign ((x number))
  (cond ((eql x 0) 0)
        ((< x 0) -1)
        (t 1)))

(defmethod delete-last-entry ((lst list))
  (reverse (cdr (reverse lst))))

(defmethod signature ((x rational))
  (apply #'+ (map 'list
                  #'sign
                  (calc-cont-frac (- x)))))

(defmethod signature2 ((x rational))
  (apply #'+ (map 'list
                  #'sign
                  (delete-last-entry
                    (calc-cont-frac x)))))

(defmethod phase-int ((x rational))
  (+ (- (* 3 
           (signature2 x)))
     (numerator x)))
                     
(defmethod phase-factor ((x rational) (k integer))
  (exp (/ (* -2 Pi #C(0 1) (phase-int x)) (central-charge k)
          24)))
