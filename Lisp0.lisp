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
		(error "No instance of *undefined*!")))