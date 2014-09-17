(defun undefined-helper (x)
	(cond 
		((not x) nil)
		((listp x)
			(or (undefined-helper (car x)) (undefined-helper (cdr x))))
		((eql x '*undefined*) t)
		(t nil)))

(defun undefined (x)
	(if (undefined-helper x)
		t
		(format t "**No instance of *undefined*!")))

(defun n-occurrences-helper (atm lst)
	(cond
		((not lst) 0)
		((listp lst)
			(+ (n-occurrences-helper atm (car lst)) 
				(n-occurrences-helper atm (cdr lst))))
		((eql lst atm) 1)
		(t 0)))

(defun n-occurrences (atm lst)
	(cond
		((not (atom atm)) 
			(format t "**First argument should be an atom, but was ~a~%" atm))
		((not (listp lst)) 0)
		(t (n-occurrences-helper atm lst))))

(defun valid_mono? (mono)
	(let ((c (first mono)) (x (second mono)) (i (third mono)))
		(cond
			((not (listp mono)) 
				(format t "**Argument must be a list.~%"))
			((not (= (list-length mono) 3)) 
				(format t "**Argument must have 3 elements.~%"))
			((not (numberp c)) 
				(format t "**First element in list must be a number.~%"))
			((numberp x) 
				(format t "**Second element in list cannot be a number.~%"))
			((listp x) 
				(format t "**Second element in list cannot itself be a list.~%"))
			((not (integerp i)) 
				(format t "**Third element in list must be a positive or negative integer.~%"))
		(t t))
	))

(defun deriv1 (mono)
	(if (valid_mono? mono)
		(let ((c (first mono)) (x (second mono)) (i (third mono)))
			(cond
				((or (= c 0) (= i 0)) (cons 0 (cons x (cons 0 nil))))
				(t (cons (* i c) (cons x (cons (- i 1) nil)))))
			)))

(defun valid_poly_helper (poly)
	(cond
		((not poly) (format t "Polynomial cannot be empty!"))
		((and (listp poly) (valid_mono? (car poly))) 
			(valid_poly? poly (second (car poly))))
		(t (format t "Polynomial must be a list of monomials!"))))

(defun valid_poly? (poly var)
	(cond
		((not poly) t)
		(t (and (and (valid_mono? (car poly)) 
							(eql var (second (car poly))))
						(valid_poly? (cdr poly) var)))))

(defun delete_zeroes (poly)
	(cond
		((not poly) nil)
		((and (eq (third (car poly)) 0) (eq (first (car poly)) 0))
			(delete_zeroes (cdr poly)))
		(t (cons (car poly) (delete_zeroes (cdr poly))))))

(defun simplify (poly)
	(if (valid_poly? poly)
		(regularize (delete_zeroes (sort poly #'> :key #'third)))))