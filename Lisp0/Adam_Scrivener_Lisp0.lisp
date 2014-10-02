;determines if x has an instance of *undefined* in it.
(defun undefined-helper (x)
	(cond 
		((not x) nil)
		((listp x)
			(or (undefined-helper (car x)) (undefined-helper (cdr x))))
		((eql x '*undefined*) t)
		(t nil)))

;returns error msg if no *undefined*
(defun undefined (x)
	(if (undefined-helper x)
		t
		(format t "**No instance of *undefined*!~%")))

;counts number of occurrences of atm in lst
(defun n-occurrences-helper (atm lst)
	(cond
		((not lst) 0)
		((listp lst)
			(+ (n-occurrences-helper atm (car lst)) 
				(n-occurrences-helper atm (cdr lst))))
		((eql lst atm) 1)
		(t 0)))

;makes sure atm is an atom. If lst is not list, return 0, else helper.
(defun n-occurrences (atm lst)
	(cond
		((not (atom atm)) 
			(format t "**First argument should be an atom, but was ~a~%" atm))
		((not (listp lst)) 0)
		(t (n-occurrences-helper atm lst))))

;determines if mono is a valid monomial, i.e. it is a list with 3 elements,
;the first element is a number, the second is not a number or a list (i.e. a variable of some kind)
;and the third is a pos/neg integer.
(defun valid_mono? (mono)
	(cond
		((not (listp mono)) 
			(format t "**Argument must be a list.~%"))
		((not (= (list-length mono) 3)) 
			(format t "**Argument must have 3 elements.~%"))
		((not (numberp (first mono))) 
			(format t "**First element in list must be a number.~%"))
		((numberp (second mono)) 
			(format t "**Second element in list cannot be a number.~%"))
		((listp (second mono)) 
			(format t "**Second element in list cannot itself be a list.~%"))
		((not (integerp (third mono))) 
			(format t "**Third element in list must be a positive or negative integer.~%"))
	(t t)
	))

;if mono is valid, returns the derivative. If c or i is 0, returns (0 x 0)
(defun deriv1 (mono)
	(if (valid_mono? mono)
		(let ((c (first mono)) (x (second mono)) (i (third mono)))
			(cond
				((or (= c 0) (= i 0)) (cons 0 (cons x (cons 0 nil))))
				(t (cons (* i c) (cons x (cons (- i 1) nil)))))
			)))

;checks if polynomial is of the right type. i.e., is a list, not nil, contains
;monomials, and has only 1 variable.
(defun valid_poly_helper (poly)
	(cond
		((not poly) (format t "Polynomial cannot be empty!~%"))
		((not (and (listp poly) (valid_mono? (car poly)))) 
			(format t "Polynomial must be a list and contain monomials!~%"))
		((not (valid_poly? poly (second (car poly))))
			(format t "Polynomial must have only 1 variable!~%"))
		(t t)))

;checks to see if poly has 1 variable.
(defun valid_poly? (poly var)
	(cond
		((not poly) t)
		(t (and (and (valid_mono? (car poly)) 
						(eql var (second (car poly))))
				(valid_poly? (cdr poly) var)))))

;deletes all instances of (0 x ?)
(defun delete_zeroes (poly)
	(cond
		((not poly) nil)
		((= (first (car poly)) 0)
			(delete_zeroes (cdr poly)))
		(t (cons (car poly) (delete_zeroes (cdr poly))))))

;combines all terms with the same degree. assumes it is sorted already.
;assumes 'deg' is the degree of the first monomial when the function first is
;called. Uses the variable "accum" to keep track of putting together all like-
;degree monomials.
(defun regularize (poly deg accum)
	(cond
		((not poly) (cons accum nil))
		((not (= deg (third (car poly)))) 
			(cons accum (regularize (cdr poly) (third (car poly)) (car poly))))
		((not (not accum)) (regularize (cdr poly) deg 
					(cons (+ (first accum) (first (car poly))) 
					(cons (second accum)
					(cons (third accum) nil)))))
		(t (regularize (cdr poly) deg (car poly)))))

;if poly is a valid polynomial, sort it by largest degree first,
;regularize it, then delete zeroes.
;returns nil poly if the simplified poly is the 0 polynomial.
(defun simplify (poly)
	(if (valid_poly_helper poly)
		(let ((sorted_poly (sort poly #'> :key #'third)))
		(delete_zeroes (regularize sorted_poly (third (car sorted_poly)) nil)))))

;derives all monos in a poly. Poly is assumed to be valid at start,
;since this function is only called by deriv2, which checks that.
(defun derive_poly (poly)
	(if (not poly) nil
		(cons (deriv1 (car poly)) (derive_poly (cdr poly)))))

;derives all monos, then simplifies.
(defun deriv2 (poly)
	(if (valid_poly_helper poly)
		(simplify (derive_poly poly))))

;prints tree in left to right order.
(defun tree-yield_helper (tree)
	(cond
		((not tree) nil)
		((listp tree) (append (tree-yield_helper (car tree)) 
								(tree-yield_helper (cdr tree))))
		(t (cons tree nil))))

;if tree is an atom, returns it. otherwise, calls tree-yield_helper.
(defun tree-yield (tree)
	(if (atom tree)
		tree
		(tree-yield_helper tree)))