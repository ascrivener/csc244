(defun test (patt expr)
	(progn
		(format t "Matching ~a with ~a~%" patt expr)
		(format t "Result: ~a~%" (match-expr patt expr))
		(format t "SUCCESS!~%~%")))

(defun run () 
(progn (load "ascriven_src.lisp")

;;test cases from the project description
(test '(_? b c) '(b c))

(test '(_? b c) '(a a b c))


(test '(a b c) '(a b c))

(test '(_? b c) '(a b c))

(test '(_! b c) '(b c))

(test '(_! b c) '((a b c) b c))

(test '_! '(a b c))

(test '(_*) '(a b c))

(test '(a _* _?) '(a b (c d) e))

(test '(a _*) '(a b (c d) e))

(test '(a (b _+ _*) _*) '(a (b (c d) e) f g))

;;complex test cases to show that everything works

(test '(_? _+ _! (_!x _?x _!y) _*) '(a b c d (e f g) h))

(test '(a (b _+ _*) _*) '(a (b (c d) e) f g))

(test '(_? _?x _?y _?z _*a _*b _*c a) '(a)) 

(test '(_* _* _* _* _* _* _*) '(a b c d e f g h))

(test '(_* (_*x (_*y (_+))) a b _?) '(() () a (b c d (e f) ((g))) a b))



;corner cases
(test '() '())

(test 'a 'b)

(test '() '(()))

(test '(()) '(()))

(test '(()) '())

(test 'a '())

;;this one was tricky, getting _! to bind to the 
;;sequence a b c d rather than the list (a b c d)
(test '_! '(a b c d))

(test '(_!) '(a b c d))

(test '(_+) '(a b c d))

(test '_+ '(a b c d))

;just showing that _+ works
(test '(_+ _+ _+ _+) '(a b c d))

(test '(_? _+ _+ _+ _+) '(a b c d))

;should not work
(test '(_? _+ _+ _+ _+ _+) '(a b c d))

(test '(_? _+ b _+ _d) '(a b c d))

(test '(_? _+ b _+ d) '(a b c d))

(test '(_? _+ b _* d) '(a b c d))
))