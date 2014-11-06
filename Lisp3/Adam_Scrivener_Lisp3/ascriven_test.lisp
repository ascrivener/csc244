; (defun mem (x lst)
; 	(cond
; 		((null lst) nil)
; 		((equal (car lst) x) t)
; 		(t (mem x (cdr lst)))))

(defun subset? (lst1 lst2)
	(cond
		((null lst1) t)
		((mem (car lst1) lst2) (subset? (cdr lst1) lst2))
		(t nil)))

(defun set-equal (lst1 lst2)
	(and (subset? lst1 lst2) (subset? lst2 lst1)))

(defun matchtest (patt expr result)
	(progn
		(format t "Matching ~a with ~a~%" patt expr)
		(format t "Result: ~a~%" (match-expr patt expr))
		(if (or (equal result (match-expr patt expr))
				(set-equal result (match-expr patt expr)))
			(format t "SUCCESS!~%~%")
			(format t "FAILURE!~%~%"))))

(defun subtest (bindings expr result)
	(progn
		(format t "Substituting bindings: ~a~%In expression: ~a~%" bindings expr)
		(format t "Result: ~a~%" (subst-bindings bindings expr))
		(if (or (equal result (subst-bindings bindings expr))
				(set-equal result (subst-bindings bindings expr)))
			(format t "SUCCESS!~%~%")
			(format t "FAILURE!~%~%"))))

(defun ruletest (expr)
	(progn
		(format t "Input: ~a~%" expr)
		(if (try-rules *rules* expr)
			(format t "Response: ~a~%~%" (try-rules *rules* expr))
			(format t "ERROR: INPUT NOT RECOGNIZED~%~%"))))

(defun run () 
(progn (load "C:/Users/Adam/Code/csc244/Lisp3/Adam_Scrivener_Lisp3/ascriven_src.lisp")

	(forward-inferences *rules* *facts*)

))