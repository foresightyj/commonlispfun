(defun add-one (x)
  (+ x 1))

(defun show-squares (start end)
  (do ((i start (+ i 1)))
      ((> i end) 'done)
    (format t "~A ~A~%" i (* i i))))

(defun show-squares-2 (start end)
  (if (> start end)
      'done
      (progn
	(format t "~A ~A~%" start (* start start))
	(show-squares-2 (+ start 1) end))))

(defun my-fourth (lst)
  (car (cdr (cdr (cdr lst)))))

(defun summit (lst)
  (if (null lst)
      0
      (let ((x (car lst)))
	(if (null x)
	    (summit (cdr lst))
	    (+ x (summit (cdr lst)))))))

(defun position+ (currpos lst)
  (if (null lst)
      'done
      (cons (+ currpos (car lst))
	    (position+ (+ 1 currpos) (cdr lst)))))

(defun pos+ (lst)
  (position+ 0 lst))

(defun show-dots (lst)
  (if (consp lst)
      (progn
	(format t "(~A." (car lst))
	(show-dots (cdr lst))
	(format t ")"))
      (format t "~A" lst)))


(defun bin-search(obj vec)
  (let ((len (length vec)))
    (and (not (zerop len))
	 (finder obj vec 0 (- len 1)))))

(defun finder (obj vec start end)
  (format t "~A ~A ~A ~A~%" obj vec start end)
  (if (equal start end)
      start
      (let ((midindex (round (/ (+ start end) 2))))
	(let ((midval (svref vec midindex)))
	  (cond ((< obj midval)
		 (finder obj vec start midindex))
		((> obj midval)
		 (finder obj vec (+ midindex 1) end))
		(t midindex))))))


   
(labels ((add-ten (x) (+ x 10))
	 (consa (x) (cons 'a x)))
  (format t "~A" (consa (add-ten 10))))


(let ((counter 0))
  (defun resetc ()
    (setf counter 0))
  (defun stampc ()
    (setf counter (+ counter 1))))

(defun iter-fibo (n)
  ( do ((i n (- i 1))
	(f1 1 (+ f1 f2))
	(f2 1 f1))
       ((<= i 1) f1)))



(defmacro nil! (x)
  `(setf ,x nil))

(defun make-queue ()
  (cons nil nil))

(defun enqueue (obj q)
  (if (null (car q))
      (setf (car q) (setf (cdr q) (list obj)))
      (setf (cdr q) (setf (cdr (cdr q)) (list obj))))
  (car q))

(defun dequeue (q)
  (pop (car q)))

(defmacro while (test &rest body)
  `(do ()
       ((not ,test) nil)
     ,@body))

(defun quick-sort (vec starti endi)
  (let ((pivotv (svref vec (round (+ starti endi) 2)))
	(lefti starti)
	(righti endi))
    (while (<= lefti righti)
      (while (> pivotv (svref vec lefti)) (incf lefti))
      (while (< pivotv (svref vec righti)) (decf righti))
      (when (<= lefti righti)
	(rotatef (svref vec lefti) (svref vec righti))
	(incf lefti)
	(decf righti)))

    (if (> (- righti starti) 1) (quick-sort vec starti righti))
    (if (> (- endi lefti) 1) (quick-sort vec lefti endi))))

