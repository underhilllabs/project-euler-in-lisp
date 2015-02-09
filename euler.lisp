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

(defun find-max-chain-nums(n)
  "Finds the longest chain and prints it out."
  (let ((max-chains 1)
        (max-chain-num 1))
    (mapc (lambda (num-chains num) 
            (when (> num-chains max-chains)
              (setf max-chains num-chains)
              (setf max-chain-num num)))
          (mapcar #'collatz-chain-num (number-sequence 1 n))
          (number-sequence 1 n))
    (format nil "The max chains count was: ~a, created by ~a" max-chains max-chain-num)))
;;(*max-chain-num*)
;;(*max-chains*)
(format nil "The max chains count was: ~a" *max-chains*)
(find-max-chain-nums 1000)
(defun find-max-chain (n)
  (loop for x from 1 below (1+ n)
   for i in 
       (mapcar #'collatz-chain-num (number-sequence 1 n))
  maximize (car (cons i x))))
;(find-max-chain 10000)

;;(find-max-chain-num 1000)
;;(print *max-chain-num*)
;;(print *max-chains*)
;;(mapcar #'collatz-chain-num (number-sequence 1 100))

;; How to define a macro
;; flip flop flop
(defmacro my-when (condition &rest body)
  `(if ,condition (progn ,@body)))

(defmacro my-unless (condition &rest body)
  `(if (not ,condition) (progn ,@body)))

(defun char-num (c)
  (- (char-code c) 64))
;;(char-num #\A)
(defun word-sum (word)
  (loop for c across word 
       summing (char-num c)))

(defun multiply-ord-word-sum (num word)
  (* num (word-sum word)))

(defun sort-and-total-names()
  (let ((names (read (open "/home/bart/projects/project-euler-in-lisp/p22_names.txt"))))
    (loop 
       for w in (sort names #'string-lessp)
       and x = 1 then (+ 1 x) 
       summing (multiply-ord-word-sum x w))))
;(sort-and-total-names)

;(pal-num-p 12321)
(defun find-max-pal-product ()
  (loop
     for x from 100 to 999
     maximize (loop for y from 100 to 999
                when (pal-num-p (* x y))
                maximize (* x y))))
;(find-max-pal-product)
;(parse-integer "10")

(defun sum-digits-quint (n)
  (reduce #'+
          (mapcar (lambda(n)
                    (expt n 5))
                  (loop for c across (write-to-string n) collect (digit-char-p c)))))
(defun sum-quint-equal-p (n)
  "Test if the number equals the sum of the number's digits taken to power of 5."
  (= n (sum-digits-quint n)))
(defun sum-quint-equal-range (n)
  "The sum of all numbers to n that equal the sum of the number's digits to the 5th"
  (reduce #'+
          (remove-if-not #'sum-quint-equal-p (number-sequence 2 n))))
;(sum-quint-equal-range 2000000)

(defun sum-digits-quad (n)
  (reduce #'+
          (mapcar (lambda(n)
                    (expt n 4))
                  (loop for c across (write-to-string n) collect (digit-char-p c)))))
(defun sum-quad-equal-p (n)
  (= n (sum-digits-quad n)))

(defun sum-quad-equal-range (n)
  (remove-if-not #'sum-quad-equal-p (number-sequence 1 n)))

;(sum-quad-equal-range 10000)
;(sum-digits-quint 123)
;(sum-digits-quad 8208)

;(word-sum "SEAMUS")

;; (my-when t
;;          (prin1 "it works")
;;          (prin1 "hurray!"))

;; fibonacci
;; (loop repeat 5 
;;       for x = 0 then y
;;       and y = 1 then (+ x y)
;;       collect (+ x y)) 
;(mapcar #'collatz-chain-num (number-sequence 1 10))

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

