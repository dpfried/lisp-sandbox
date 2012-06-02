(defun range (n &optional (start 0) (step 1))
  (if (< start n)
      (cons start (range n (+ start step) step))))

(defun last1 (lst)
  "returns last element (not cons pair) in a list"
  (car (last lst)))

(defun single (lst)
  "returns true iff lst is a list with exactly one item"
  (and (consp lst) (not (cdr lst))))

(defun append1 (lst obj)
  "append obj to the end of list lst"
  (append lst (list obj)))

(defun conc1 (lst obj)
  "append obj to the end of list lst, destructively"
  (nconc lst (list obj)))

(defun mklist (obj)
  "make a list of obj if it is not already"
  (if (listp obj) obj (list obj)))

(defun longer (x y)
  "returns true iff x is longer than y"
  (labels ((compare (x y)
	     (and (consp x)
		  (or (null y)
		      (compare (cdr x) (cdr y))))))
    (if (and (listp x) (listp y))
	(compare x y)
	(> (length x) (length y)))))

(defun filter (fn lst)
  "returns a list of all logically true values of applying fn to each item in lst"
  (let ((acc nil))
    (dolist (x lst)
      (let ((val (funcall fn x)))
	(if val (push val acc))))
    (nreverse acc)))

(defun group (source n)
  "group the elements in source in lists of length n"
  (if (zerop n) (error "zero length"))
  (labels ((rec (source acc)
	     (let ((rest (nthcdr n source)))
	       (if (consp rest)
		   (rec rest (cons (subseq source 0 n) acc))
		   (nreverse (cons source acc))))))
    (if source (rec source nil) nil)))
	   
(defun flatten (lst)
  "return a list of all atoms in lst and its sublists"
  (labels ((rec (x acc)
	     (cond ((null x) acc)
		   ((atom x) (cons x acc))
		   (t (rec (car x) (rec (cdr x) acc))))))
    (rec lst nil)))

(defun prune (test tree)
  "remove all items that don't satisfy the test predicate from the tree"
  (labels ((rec (tree acc)
	     (cond ((null tree) (nreverse acc))
		   ((consp (car tree))
		    (rec (cdr tree)
			 (cons (rec (car tree) nil) acc)))
		   (t (rec (cdr tree)
			   (if (funcall test (car tree))
			       acc
			       (cons (car tree) acc)))))))
    (rec tree nil)))

(defun before (x y lst &key (test #'eql))
  "is x found before y in the lst
   returns true iff we don't find y before finding x"
  (and lst
       (let ((first (car lst)))
	 (cond ((funcall test y first) nil)
	       ((funcall test x first) lst)
	       (t (before x y (cdr lst) :test test))))))

(defun after (x y lst &key (test #'eql))
  "is x found after y in the lst
   returns true iff x and y are both in the list,
   and x occurs after y"
  (let ((rest (before y x lst :test test)))
    (and rest (member x rest :test test))))

(defun duplicate (obj lst &key (test #'eql))
  "returns true iff obj appears at least two times
   in lst"
  (member obj (cdr (member obj lst :test test))
	  :test test))

(defun split-if (fn lst)
  "splits the list at the first point fn returns true,
   useful for ordered lists"
  (let ((acc nil))
    (do ((src lst (cdr src)))
	((or (null src) (funcall fn (car src)))
	 (values (nreverse acc) src))
      (push (car src) acc))))

(defun most (fn lst)
  "a combination of argmax and max on the lst, by fn"
  (if (null lst)
      (values nil nil)
      (let* ((wins (car lst))
	     (max (funcall fn wins)))
	(dolist (obj (cdr lst))
	  (let ((score (funcall fn obj)))
	    (when (> score max)
	      (setq wins obj
		    max score))))
	(values wins max))))

(defun best (fn lst)
  "if fn defines a total order on the elements of lst, returns the greatest element"
  (if (null lst)
      nil
      (let ((wins (car lst)))
	(dolist (obj (cdr lst))
	  (if (funcall fn obj wins)
	      (setq wins obj)))
	wins)))

(defun mostn (fn lst)
  "like most but will return a list of all objects with max score instead
of the first found"
  (if (null lst)
      (values nil nil)
      (let ((max (funcall fn (car lst)))
	    (wins (list (car lst))))
	(dolist (obj (cdr lst))
	  (let ((score (funcall fn obj)))
	  (cond ((> score max ) (setq max score
				      wins (list obj)))
		((= score max) (push obj wins)))))
	(values (nreverse wins) max))))

(defun mapa-b (fn a b &optional (step 1))
  "map fn on values between a and b, inclusive, separated by step"
  (do* ((i a (+ i step))
	(acc nil (cons (funcall fn i) acc)))
       ((>= i b) (nreverse acc))))

(defun map0-n (fn n)
  "map fn on values from 0 to n inclusive"
  (mapa-b fn 0 n))

(defun map1-n (fn n)
  "map fn on values from 1 to n inclusive"
  (mapa-b fn 1 n))

(defun map-> (fn start test-fn succ-fn)
  "in haskell:
   map fn $ takeWhile (not . test-fn) $ iterate fn start"
  (do ((i start (funcall succ-fn i))
       (result nil))
      ((funcall test-fn i) (nreverse result))
    (push (funcall fn i) result)))

(defun mappend (fn &rest lsts)
  "a non-destructive mapcan"
  (apply #'append (apply #'mapcar fn lsts)))

(defun mapcars (fn &rest lsts)
  "fewer processor cycles but more bytes consed than graham's"
  (mappend (lambda (lst) (mapcar fn lst)) lsts))

(defun mapcars-grahams (fn &rest lsts)
  (let ((result nil))
    (dolist (lst lsts)
      (dolist (obj lst)
	(push (funcall fn obj) result)))
    (nreverse result)))

(defun rmapcar (fn &rest args)
  "mapcar for trees"
  (if (some #'atom args)
      (apply fn args)
      (apply #'mapcar
	     #'(lambda (&rest args)
		 (apply #'rmapcar fn args))
	     args)))

;;; I/O Functions
(defun readlist (&rest args)
  "calls read-line with the specified args, wraps what's returned in a list"
  (values (read-from-string
	   (concatenate 'string "("
			(apply #'read-line args)
			")"))))

(defun prompt (&rest args)
  "ex: (prompt \"Enter a number between ~a and ~a. ~%>> \" 1 10)"
  (apply #'format *query-io* args)
  (read *query-io*))

(defun break-loop (fn quit &rest args)
  "emulate the top-level. apply fn to each input, terminate if (quit input)"
  (format *query-io* "Entering break-loop. ~%")
  (loop
       (let ((in (apply #'prompt args)))
	 (if (funcall quit in)
	     (return)
	     (format *query-io* "~A~%" (funcall fn in))))))

;;; 4.7 Symbols and Strings
(defun mkstr (&rest args)
  "make a string out of the args"
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))

(defun symb (&rest args)
  " this will be handy for macros"
  (values (intern (apply #'mkstr args))))

(defun reread (&rest args)
  "takes a series of objects, prints and rereads them"
  (values (read-from-string (apply #'mkstr args))))

(defun explode (sym)
  "(explode 'bomb) -> '(B O M B)"
  (map 'list #'(lambda (c)
		 (intern (make-string 1
				      :initial-element c)))
       (symbol-name sym)))

;;; Destructive equivalents
(defvar *!equivs* (make-hash-table))

(defun ! (fn)
  (or (gethash fn *!equivs*) fn))

(defun def! (fn fn!)
  (setf (gethash fn *!equivs*) fn!))

;;; returning functions
(defun memoize (fn)
  (let ((cache (make-hash-table :test #'equal)))
    #'(lambda (&rest args)
	(multiple-value-bind (val win) (gethash args cache)
	  (if win
	      val
	      (setf (gethash args cache)
		    (apply fn args)))))))

(defun compose (&rest fns)
  (if fns
      (let ((fn1 (car (last fns)))
	    (fns (butlast fns)))
	#'(lambda (&rest args)
	    (reduce #'funcall fns
		    :from-end t
		    :initial-value (apply fn1 args))))
      #'identity))

(defun fif (if then &optional else)
  "functional if statement"
  #'(lambda (x)
      (if (funcall if x)
	  (funcall then x)
	  (if else (funcall else x)))))

(defun fint (fn &rest fns)
  "intersection of predicates
  (fint #'p #'q #'r) == (lambda (x) (and (p x) (q x) (r x)))"
  (if (null fns)
      fn
      (let ((chain (apply #'fint fns)))
	#'(lambda (x)
	    (and (funcall fn x) (funcall chain x))))))

(defun fun (fn &rest fns)
  "union of predicates
   (fun #'p #'q #'r) == (lambda (x) (or (p x) (q x) (r x)))"
  (if (null fns)
      fn
      (let ((chain (apply #'fun fns)))
	      #'(lambda (x)
		  (or (funcall fn x) (funcall chain x))))))