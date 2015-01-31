(defun divp (d n)
  (=
   (mod n d) 0))

(defun ldf (k n)
  (cond ((divp k n) k)
        ((> (expt k 2) n) n)
        (t (ldf (1+ k) n))))

(defun ld (n)
  (ldf 2 n))

(defun prime0 (n)
  (=
   (ld n) n))

(defun mult-3-or-5-p (n)
  (cond ((= 0 (mod n 3)) t)
        ((= 0 (mod n 5)) t)
        (t nil)))

(defun number-sequence (start stop &optional (step 1))
  (loop for n from start to stop by step
       collect n))

(defun mult-3-or-5-range (n)
  (remove-if-not #'mult-3-or-5-p (number-sequence 1 n)))


;; Testing
;;(mult-3-or-5-range 100)
;;(number-sequence 1 100 2)
;;(mod 3 13)
;;(mult3or5p 13)
;;(divp 5 235)
;;(ld 45905909)
;;(prime0 132)
