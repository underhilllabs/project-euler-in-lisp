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

(defun fizbuzz-num (n)
  (cond ((and (= 0 (mod n 3)) (= 0 (mod n 5))) 'fizzbuzz)
        ((= 0 (mod n 3)) 'fizz)
        ((= 0 (mod n 5)) 'buzz)
        (t n)))

(defun fizbuzz (n)
  (mapcar #'fizbuzz-num (number-sequence 1 n)))

(defun fibo (n)
  (cond ((= 0 n) 1)
        ((= 1 n) 1)
        (t (+ (fibo (- n 1)) (fibo (- n 2))))))

(defun fibo-list (n)
  (mapcar #'fibo (number-sequence 1 n)))

(defun factorial (n)
  (cond ((= 0 n) 1)
        (t (reduce #'* (number-sequence 1 n)))))

(defun number-to-list (n)
  (loop for c across (write-to-string n) collect (digit-char-p c)))

(defun factors (n)
  (loop for c from 2 below n when (divp c n) collect c))

(defun prime-factors (n)
  (remove-if-not #'prime0 (factors n)))

(defun palindrome-p (s)
  (equal s (reverse s)))

(defun pal-num-p (n)
  (equal (write-to-string n) (reverse (write-to-string n))))

(defun div-by-1-20-p (n)
  (loop for c from 11 to 20 always (divp c n)))

(defun read-lines-into-sum (file-name)
  (let ((in (open file-name :if-does-not-exist nil))
        (total 0))
    (when in
      (loop for line = (read-line in nil)
            while line do (setf total (+ (parse-integer line) total)))
      (close in))
    total))

(defun read-lines-into-list (file-name)
  (let ((in (open file-name :if-does-not-exist nil))
        (my-list ()))
    (when in
      (loop for line = (read-line in nil)
            while line do (setf my-list (cons (parse-integer line) my-list)))
      (close in))
    my-list))

(defun read-lines-into-number (file-name)
  (let ((in (open file-name :if-does-not-exist nil))
        (my-list ()))
    (when in
      (loop for line = (read-line in nil)
            while line do (append my-list (list (parse-integer line))))
      (close in))
    (concatenate 'string my-list)))

(defun square (x) (* x x))

(defun diff-sum-squares-and-squared-sum (n)
  (- (square (reduce #'+ (number-sequence 1 n)))
     (reduce #'+ (mapcar #'square (number-sequence 1 n)))))

(defun odd (n)
  (= (mod n 2) 1))

(defun collatz-num (n)
  (cond ((= n 1) 1)
        ((odd n) (1+ (* 3 n)))
        (t (/ n 2))))

(defun collatz-chain (n)
  (setf x (collatz-num n))
  (cond ((= n 1) nil)
        (t (cons x (collatz-chain x)))))

(defun collatz-chain-num (n)
  (setf x (collatz-num n))
  (cond ((= n 1) 1)
        (t (1+ (collatz-chain-num x)))))

(defun collatz-list (max)
  (mapcar (lambda(x) (cons x (list (collatz-chain-num x)))) (number-sequence 1 max)))

(defun self-powers (n)
  (reduce #'+ (mapcar (lambda (x) (expt x x)) (number-sequence 1 n))))

(defparameter *max-chains* 1)
(defparameter *max-chain-num* 1)
(defun find-max-chain-num ()
  (apply 
   (lambda (num num-chains) 
     (when (> num-chains *max-chains*)
       (setf *max-chains* num-chains)
       (setf *max-chain-num* num)))
  '(4 22)))

;(mapcar #'collatz-chain-num (number-sequence 1 10))

(find-max-chain-num)
(print *max-chain-num*)
(print *max-chains*)

;(collatz-list 100)

;;(collatz-num 1)
;;(collatz-chain 3)
;;(collatz-chain-num 13)
;;(read-lines-into-number "nums8.txt")
;;(reduce #'+ (read-lines-into-list "nums.txt"))
;;(pal-num-p 1232)
;;(palindrome-p "lapal")
;;(palindrome-p "1232")
;;(prime-factors 600851475143)
;;(reduce #'+ (mult-3-or-5-range 1 999))
;;(reduce #'+ (number-to-list (factorial 1000)))
;;(reduce #'+ (number-to-list (expt 2 1000)))

