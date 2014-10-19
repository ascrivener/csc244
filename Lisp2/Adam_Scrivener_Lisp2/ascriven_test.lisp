(defun matchtest (patt expr result)
	(progn
		(format t "Matching ~a with ~a~%" patt expr)
		(format t "Result: ~a~%" (match-expr patt expr))
		(if (equal result (match-expr patt expr))
			(format t "SUCCESS!~%~%")
			(format t "FAILURE!~%~%"))))

(defun subtest (bindings expr result)
	(progn
		(format t "Substituting bindings: ~a~%In expression: ~a~%" bindings expr)
		(format t "Result: ~a~%" (subst-bindings bindings expr))
		(if (equal result (subst-bindings bindings expr))
			(format t "SUCCESS!~%~%")
			(format t "FAILURE!~%~%"))))

(defun ruletest (expr)
	(progn
		(format t "Input: ~a~%" expr)
		(if (try-rules *rules* expr)
			(format t "Response: ~a~%~%" (try-rules *rules* expr))
			(format t "ERROR: INPUT NOT RECOGNIZED~%~%"))))

(defun run () 
(progn (load "C:/Users/Adam/Code/csc244/Lisp2/Adam_Scrivener_Lisp2/ascriven_src.lisp")

(matchtest '((* a b)) '(a b a b) '((* a b a b)))

(matchtest '((* a b)) '(a b a b c) nil)

(matchtest '((* a b) c) '(a b a b c) '((* a b a b)))

(matchtest '((* a ~ c)) '(a b a b c) nil)

(matchtest '((*1 a ~ c) (*2 c ~ a)) '(a a c c) '((*2 c c) (*1 a a)))

(matchtest '(!1 (a (!2 ~ b)) ((!3 ~ a) b)) '(a a) '((!1 (a a)) (!2 a)))

(matchtest '(!1 (a (!2 ~ b)) ((!3 ~ a) b)) '(a b) nil)

(matchtest '(!1 (a (*2 b)) (b (*2 a))) '(a b b b b) '((!1 (a b b b b)) (*2 b b b b)))

(matchtest '((?1 a) (!1 b) (*1 c) (*2 d)) '(b c c c d d) '((*2 d d) (*1 c c c) (!1 b) (?1)))

(matchtest '((+x a) (*1 ~ a) (!1 a) (?1 a)) '(a a a a a) '((?1 a) (!1 a) (*1) (+x a a a)))

(ruletest '(I hate my new boss))

(ruletest '(I am sad))

(ruletest '(I am potato))

(ruletest '(I will be happy))

(ruletest '(I am very upset about my grades))

(ruletest '(I am so very very very very upset about my grades))

(ruletest '(I am very very very happy about my relationship))

(ruletest '(I am happy about my weight))

(ruletest '(I have been silly))

(ruletest '(I would be rich))

(ruletest '(I am from Italy))

(ruletest '(Yo soy de Italia))

(ruletest '(Would you like to talk about our feelings?))

(ruletest '(How about my love of basket weaving?))

(subtest '((! (b)) (_* c)) '((a) ! (_*)) '((a) (b) (c)))

(subtest '((_! hate) (! my) (* present job)) '(Why do you _! your * ?)
				'(Why do you hate your present job ?))

(subtest '((_!1 a) (*1 b c d) (*2) (+x e) (?1 f))
		'(_!1 *1 *2 +x ?1 g ?) '(a b c d e f g ?))

))