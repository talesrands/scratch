;; Tales Rands

(defun kruskal (vert are)
  (let ((sets (mapcar #'(lambda (x) (list x)) vert))
	 (edge (sort are #'(lambda (a b) (< (third a) (third b))))))
    (use-edge edge sets)))


(defun use-edge (edge sets &optional (in-use nil))
  (if edge
      (let ((set1 (find-set (car (car edge)) sets))
	    (set2 (find-set (second (car edge)) sets)))
	(if (not (equal set1 set2))
	    (use-edge (cdr edge)
		      (unite-subsets sets set1 set2)
		      (push (car edge) in-use))
	    (use-edge (cdr edge) sets in-use)))
      (values (sum-list (mapcar #'third in-use)) in-use)))


(defun unite-subsets (sets set1 set2)
  (cons (append set1 set2)
	(remove set2 (remove set1 sets :test #'equal) :test #'equal)))


(defun find-set (vert sets)
  (first-not-nil (mapcar #'(lambda (alist)
			      (if (member vert alist)
				  alist
				  nil)) sets)))


(defun first-not-nil (alist)
  (if (car alist)
      (car alist)
      (first-not-nil (cdr alist))))


(defun sum-list (alist &optional (sum 0))
  (if alist
      (sum-list (cdr alist) (+ (car alist) sum))
      sum))
