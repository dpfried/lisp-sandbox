#|
carnivore
fly
domesticated
walk on four legs
hoofs
used for human food
rodent
predator
prey
feline
hibernate
|#

(defparameter ch-list '(
   ;(animal    1 represents having the trait, 0 represents not)
	(bear	   1 0 0 1 0 1 0 1 0 0 1)
	(dog 	   1 0 1 1 0 0 0 1 1 0 0)
	(cat 	   1 0 1 1 0 0 0 1 1 1 0)
	(chicken   1 1 1 0 0 1 0 1 1 0 0)
	(cow 	   0 0 1 1 1 1 0 0 1 0 0)
	(deer	   0 0 0 1 1 1 0 0 1 0 0)
	(elephant  0 0 0 1 0 0 0 0 1 0 0)
	(goat	   0 0 1 1 1 1 0 0 1 0 0)
	(horse	   0 0 1 1 1 1 0 0 1 0 0)
	(mouse	   1 0 1 1 0 0 1 1 1 0 0)
	(lion	   1 0 0 1 0 0 0 1 0 1 0)
	(pig	   1 0 1 0 1 1 0 1 1 0 0)
	(rat	   1 0 0 1 0 0 1 1 1 0 0)
	(sheep	   0 0 1 1 1 1 0 0 1 0 0)
	(squirrel  1 0 0 1 0 0 1 1 1 0 0)
	(tiger	   1 0 0 1 0 0 0 1 0 1 0)))

#|
(defparameter ch-list '(
(a 1 0 1 0 0)
(b 0 1 0 1 0)
(c 1 0 1 0 0)
(d 0 1 0 1 0)
(e 1 0 1 0 0)
(f 0 1 0 1 0)
(g 1 0 1 0 0)
(h 0 1 0 1 1)
(i 0 1 1 0 0)))
|#
(defun zero-list (length)
  (loop for x from 1 to length collect 0))

(defun characteristic-entropy (lst)
  "Returns a list. Each entry in the list is -1 * the entropy of its corresponding characteristic. Higher values correspond to more dissimilarity. The highest possible value is 1.0, if there is an even number of animals that have and don't have the characteristic"
  (let* ((number-characteristics (length (first lst)))
	 (number-entities (length lst))
	 (count-positive-characteristics (zero-list number-characteristics)))
    (dolist (seq lst)
      (setf count-positive-characteristics (mapcar #'+ count-positive-characteristics seq)))
    (let* ((p-of-positive (mapcar #'(lambda (x) (/ x number-entities)) count-positive-characteristics))
	   (p-of-negative (mapcar #'(lambda (x) (- 1 x)) p-of-positive)))
      (mapcar #'(lambda (x y) (if (or (= x 0) (= y 0)) 0 (* -1 (+ (* x (log x 2)) (* y (log y 2)))))) p-of-positive p-of-negative))))

(defun entropy-ch-list ()
  (characteristic-entropy (mapcar #'cdr ch-list)))

(defparameter characteristic-weights 
  (entropy-ch-list))

(defun compare-naive (animal1 animal2)
	"Compares two animals given their names (i.e. 'dog or 'cat). Values range from 0 to number-of traits, and is simply the number of traits they have in common"
	(reduce #'+ (mapcar #'(lambda (x y) (if (= x y) 1 0)) (cdr (find animal1 ch-list :key #'car)) (cdr (find animal2 ch-list :key #'car)))))

(defun compare-weighted (animal1 animal2)
  "Compares two animals given their names (i.e. 'dog or 'cat). Values range from 0 to max-similarity, with higher values corresponding to more similarity. Return the sum of the entropies of all the traits that the animals have in common. A pair that shares traits with high global entropy receive a higher similarity score than a pair that shares traits with low global entropy."
  (reduce #'+ (mapcar #'(lambda (x y e) (* (if (= x y) 1 0) e)) 
		      (cdr (find animal1 ch-list :key #'car)) 
		      (cdr (find animal2 ch-list :key #'car)) characteristic-weights)))

(defun max-similarity ()
  "Returns the similarity value of animals with the exact same characteristics (i.e. if an animal is compared to itself"
  (compare-weighted (car (car ch-list)) (car (car ch-list))))

(defun compare-all ()
  (comp-all (mapcar #'car ch-list)))

(defun comp-all (lst)
  (or (null lst)
      (dolist (elem (cdr lst))
		(format t "~A~T~A~T~A" (car lst) elem (compare-weighted (car lst) elem))
		(terpri))
      (comp-all (cdr lst))))

(defun sort-by-similarity-to (animal &key (key #'identity) (comp-fn #'compare-weighted))
  (mapcar key (sort (mapcar #'(lambda (x) (cons (funcall comp-fn (car x) animal) x)) ch-list) #'> :key #'car)))

(defun print-sorted-list (animal &key (key #'identity) (comp-fn #'compare-weighted))
  (dolist (elem (sort-by-similarity-to animal :key key :comp-fn comp-fn))
    (princ elem)
    (terpri)))
  