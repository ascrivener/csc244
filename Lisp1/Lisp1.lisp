;;input: an S-expression S
;;output: if S is an atom whose first characters
;;are _+, _!, _?, or _*, then output those two characters.
;;if S is an atom and its first 2 characters are not in this
;;list, (or if it has fewer than 2 characters),output nil. 
;;If S is a list, output nil.
;;allows us to tell when a token is a variable, and what
;;type it is., if it is.
(defun variable? (expr)
	(if (atom expr) 
		(let ((str (symbol-name expr)))
			(if (>= (length str) 2) 
				(let ((substr (subseq str 0 2)))
				(if (or (string-equal substr "_!")
						(string-equal substr "_?")
						(string-equal substr "_*")
						(string-equal substr "_+"))
					(let ((out (intern substr)))
						out)))))))

;;Input: two S-expressions patt and expr

(defun match-expr (patt expr)
	(progn  (setq table (make-hash-table :test 'eq))
			(if (match-expr-helper patt expr)
				(if (> (hash-table-count table) 0)
					(print-hash nil)
					t))))

(defun match-expr-helper (patt expr)
	(if (atom patt)
		(case (variable? patt)
			(_* (progn (setf (gethash patt table) (list expr))) t)
			(_+ (progn (setf (gethash patt table) (list expr))) t)
			(_! (progn (setf (gethash patt table) expr)) t)
			(_? (progn (setf (gethash patt table) expr)) t)
			(t (eq patt expr)))
		(if (listp expr) (match-list patt expr))))

(defun match-list (patt expr)
	(let ((tok (first patt)))
		(if (atom tok) 
			(case (variable? tok)
				(_! (if expr (_!handler patt expr tok)))
				(_? (_?handler patt expr tok))
				(_+ (if expr (_+handler patt expr tok nil)))
				(_* (_*handler patt expr tok))
				(t (if patt 
						(if (and expr (eq tok (first expr))) 
							(match-list (cdr patt) (cdr expr)))
						(not expr))))
			(if (listp (first expr)) 
				(and (match-list tok (first expr)) 
					 (match-list (cdr patt) (cdr expr)))))))


(defun _!handler (patt expr tok)
	;;tok = (first expr)
	(progn (setf (gethash tok table) (first expr))
		   (match-list (cdr patt) (cdr expr))))

(defun _?handler (patt expr tok)
	;;tok = empty
	(progn (setf (gethash tok table) "$$$EMPTY_STRING$$$")
			(if (not (match-list (cdr patt) expr))
				(_!handler patt expr tok)
				t)))

(defun _+handler (patt expr tok agg)
	;;tok = agg
	(progn (setf (gethash tok table) (cons (first expr) agg))
			(if (not (match-list (cdr patt) (cdr expr)))
				(if (cdr expr) (_+handler patt (cdr expr) tok (cons (first expr) agg)))
				t)))

(defun _*handler (patt expr tok)
	;;tok = empty
	(progn (setf (gethash tok table) "$$$EMPTY_STRING$$$")
			(if (not (match-list (cdr patt) expr))
				(_+handler patt expr tok nil)
				t)))

(defun print-hash (agg)
	(loop for v being the hash-values of table 
		using (hash-key k) 
		collect (if (and (stringp v) (string-equal v "$$$EMPTY_STRING$$$"))
						(list k)
						(case (variable? k)
							(_* (mklist (cons k nil) (reverse v)))
							(_+ (mklist (cons k nil) (reverse v)))
							(t (list k v))))))

(defun mklist (agg rest)
	(if rest
		(mklist (cons (first rest) agg) (cdr rest))
		(reverse agg)))