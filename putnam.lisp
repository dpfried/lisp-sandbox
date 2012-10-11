(defun range (start end)
  (loop for x from start to end collecting x))

(defun all-same-p (seq)
  (every (lambda (e)
	      (= e (first seq)))
	  seq))

(defun prime-power-p (k)
  (all-same-p (factor k)))

(defun bachets-prime (max)
  (let ((losing-states (list 1 0)))
    (dolist (k (range 2 max))
      (if (notany #'prime-power-p (mapcar (lambda (j)
					     (- k j))
					  losing-states))
	  (push k losing-states)))
    (reverse losing-states)))
