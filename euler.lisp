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

(defun mult3or5p (n)
  (cond ((= 0 (mod n 3)) t)
        ((= 0 (mod n 5)) t)
        (t nil)))


;;(mod 3 13)
;;(mult3or5p 13)
;; Testing
;;(divp 5 235)
;;(ld 45905909)
;;(prime0 137)
