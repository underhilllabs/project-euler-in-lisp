## Lisp Math Notes

### range 
```lisp
(defun number-sequence (start stop &optional (step 1))
	(loop for n from start to stop by step
		collect n))

(number-sequence 1 1000000)
```

### Equality
* (eq) for symbols
* (=) for numbers
* (equals) for most everything else

### Conditionals
#### cond
```lisp
(cond ((test1) result1)
      ((test2) (result2a result2b))
      (t defaultresult))
```

real example:

```lisp
  (cond ((divp k n) k)
        ((> (expt k 2) n) n)
        (t (ldf (1+ k) n))))
```

#### if

#### when/unless

#### or/and

#### case

## Lisp Notes

### defmacro

Reimplement the when macro.

```lisp
(defmacro my-when (condition &rest body)
    `(if ,condition (progn ,@body)))
```



