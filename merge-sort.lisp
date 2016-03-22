;; Author: Tales

(defun merge-sort (alist &optional (size -1))
  (let* ((size (if (< size 0)
		   (length alist)
		   size))
	 (mid (floor (length alist) 2)))
    (if (> size 1)
	(merge 'list
               (merge-sort (subseq alist 0 mid) mid)
               (merge-sort (subseq alist mid) (- size mid)) 
               #'<=)
      alist)))

(defun merge-1 (list1 list2)
  (cond ((null list1)
         list2)
        ((null list2)
         list1)
        (t (if (<= (car list1) (car list2))
               (cons (car list1) (merge-1 (cdr list1) list2))
               (cons (car list2) (merge-1 list1 (cdr list2)))))))
