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