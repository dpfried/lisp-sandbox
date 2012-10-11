(defun factor-with-prime-list (x lst)
  (and (not (= x 1))
       (let* ((l (copy-list lst))
	      (n
	       (do ((d (car l) (car l)))
		   ((or (null l) (= (mod x d) 0)) d)
		 (setf l (cdr l)))))
	 (if (= (length l) 0)
	     (list x)
	     (cons n (factor-with-prime-list (/ x n) lst))))))

(defun rle (x)
  (if (consp x)
      (compr (car x) 1 (cdr x))
      x))

(defun compr (elt n lst)
  (if (null lst)
      (list (n-elts elt n))
      (let ((next (car lst)))
	(if (eql next elt)
	    (compr elt (+ n 1) (cdr lst))
	    (cons (n-elts elt n)
		  (compr next 1 (cdr lst)))))))

(defun n-elts (elt n)
  (list elt n))

(defun prime-list (max)
  (let ((sieve (make-array (- max 1) :element-type 'integer)))
    (dotimes (ind (- max 1))
      (setf (aref sieve ind) (+ ind 2)))
					; Begin filtering
    (do ((ind 0 (1+ ind)))
	((>= ind (- max 1)))
      (and (not (= (aref sieve ind) 0))
	   (do ((n (* (+ ind 2) 2) (+ n ind 2)))
	       ((> n max))
	     (setf (aref sieve (- n 2)) 0))))
    (let ((acc nil))
      (do ((i (- max 2) (1- i)))
	  ((< i 0))
	(if (not (= 0 (aref sieve i)))
	    (setf acc (cons (aref sieve i) acc))))
      acc)))
   
(defun extend-prime-list-destructive (pl &optional (amount 1))
  (setf pl (nreverse pl))
  (dotimes (n amount)
    (do ((d 2 (+ 2 d))
	 (halt-flag)
	 (last-prime (car pl) (car pl)))
	(halt-flag)
      (when (reverse-is-prime (+ last-prime d) pl)
	(setf pl (cons (+ last-prime d) pl)
	      halt-flag t))))
  (setf pl (nreverse pl))
  pl)

	  

(defun factor (x)
  (factor-with-prime-list x (prime-list (round (sqrt x)))))

(defun force-list (x)
  (let ((g-x x))
    (if (listp g-x)
	g-x
	(list g-x))))

(defun test (n) 
  (= n (reduce #'* 
	       (force-list (factor n)))))

(defun test-loop (start stop)
  (do ((err nil (not (test x))) 
       (x start (1+ x)))
      ((or err (> x stop)) x)))

(defun number-list (start stop)
  (do ((acc nil (cons x acc))
       (x stop (1- x)))
      ((< x start) acc)))

(defun print-factors (start stop)
  (do ((i start (1+ i))
       (fl (prime-list (round (sqrt stop)))))
      ((> i stop))
    (format t "~A:~A" i (factor-with-prime-list i fl))
    (terpri)))

(defun number-of-factors (x pl &optional)
  (let ((acc 1))
    (dolist (elem (rle (factor-with-prime-list x pl)) acc)
      (setf acc (* acc (+ 1 (second elem)))))))

(defun rle-divisors (lst)
  (cond ((null lst) nil)
	((= (length lst) 1)
	 (loop for x from 0 to (cadar lst) collect (expt (caar lst) x)))
	(t (do ((agg nil) (succ-div (rle-divisors (cdr lst))) (x 0 (1+ x)))
	       ((> x (cadar lst)) agg)
	     (setf agg (append (mapcar #'(lambda (n) (* (expt (caar lst) x) n)) succ-div)  agg))))))
  
(defun divisors (x)
  (rle-divisors (rle (factor x))))

(defun proper-divisors (x)
  (remove x (rle-divisors (rle (factor x))) :count 1))

(defun divisors-pl (x pl)
  (rle-divisors (rle (factor-with-prime-list x pl))))

(defun proper-divisors-pl (x pl)
  (remove x (rle-divisors (rle (factor-with-prime-list x pl))) :count 1))

(defun is-prime (x &optional pl)
  (if (not (null pl))
      (or (bs-contains pl x)
	  (and (> x 1)
	       (not (some #'(lambda (y) (= (mod x y) 0)) pl))))
      (and (> x 1)
	   (not (some #'(lambda (y) (= (mod x y) 0)) (prime-list (ceiling (sqrt x))))))))

(defun reverse-is-prime (x pl)
  (or (r-bs-contains pl x)
      (and (> x 1)
	   (not (some #'(lambda (y) (= (mod x y) 0)) pl)))))

(defun bs-contains (lst x)
  (not (or (null lst)
	   (= -1 (bs-helper lst x 0 (1- (length lst)))))))
