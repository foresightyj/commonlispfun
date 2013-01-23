(defun our-member (obj lst)
  (if (null lst)
      nil
      (if (eql (car lst) obj)
	  lst
	  (our-member obj (cdr lst)))))

(defun getnumber (prompt)
  (format t prompt)
  (read))

(defun get-age()
  (let ((age (getnumber "What is your age~%")))
    (if (numberp age)
	age
	(get-age))))

(defun show-squares (start end)
  (do ((i start (+ i 1)))
      ((> i end) 'done)
    (format t "~A ~A~%" i (* i i))))

(defun show-squares-recur (start end)
  (if (> start end)
      'done
      (progn
	(format t "~A ~A~%" start (* start start))
	(show-squares-recur (+ start 1) end))))

(defmacro while (test &rest body)
  `(do ()
       ((not ,test))
     ,@body))

(defun quicksort (vec l r)
  (let ((i l)
	(j r)
	(p (svref vec (round (+ l r) 2))))
    (while (<= i j)
      (while (< (svref vec i) p) (incf i))
      (while (> (svref vec j) p) (decf j))
      (when (<= i j)
	(rotatef (svref vec i) (svref vec j))
	(incf i)
	(decf j)))
    (if (> (- j l) 1) (quicksort vec l 
