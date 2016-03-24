;; Solving Project Euller: problem 105
;; https://projecteuler.net/problem=105
;; Author: Tales Rands

;; Original example: https://projecteuler.net/project/resources/p105_sets.txt
;; Correct Answer: 73702

(defun string->list (str &optional (start 0) (alist nil))
  (multiple-value-bind (strn pos)
      (read-from-string str nil nil :start start)
    (if (and strn (<= pos (length str)))
	(string->list str pos (cons strn alist))
	(reverse alist))))

(defun txt2list (name)
  (with-open-file (in name)
	  (let ((res))
	    (do ((line (read-line in nil nil)
		       (read-line in nil nil)))
		((null line)
		 (reverse res))
	      (push (string->list line) res)))))

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

(defun test1 (alist sum1 sum2 taken-step total-step n)
  (if (<= taken-step total-step)
      (let ((nsum1 (+ sum1 (nth (+ taken-step 1) alist)))
	    (nsum2 (+ sum2 (nth (- n (+ 1 taken-step)) alist))))
	(if (> nsum1 nsum2)
	    (test1 alist nsum1 nsum2 (+ taken-step 1) total-step n)
	    nil))
      T))

(defun subsets (alist)
  (if (null alist) '(nil)
  (append (subsets (cdr alist))
	  (mapcar #'(lambda (sets)
		      (cons (car alist) sets))
		  (subsets (cdr alist))))))

(defun sum-list (alist &optional (sum 0))
  (if (null alist) sum
      (sum-list (cdr alist) (+ sum (car alist)))))

(defun repeated-p (alist)
  (cond
    ((null alist) nil)
     ((null (member (car alist)
		   (cdr alist)))
	    (repeated-p (cdr alist)))
     (t t)))

(defun special-p (alist)
  (let ((nlist (merge-sort alist))
	(n (length alist)))
    (if (test1 nlist
	       (car nlist) 0 0
	       (floor (- n 3) 2)
	       n)
	(not (repeated-p (mapcar #'sum-list
				 (cdr (subsets alist)))))
	nil)))

(defun projecteuller105 (alist)
  (sum-list (mapcar #'sum-list (remove-if-not #'special-p alist))))
