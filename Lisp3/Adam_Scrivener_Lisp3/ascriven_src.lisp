;;(load "C:/Users/Adam/Code/csc244/Lisp3/Adam_Scrivener_Lisp3/ascriven_src.lisp")

; (setq *rule1* '(/ (I _! (! my the)
; 			(* (!1 present stupid new) (!2 job boss room-mate)))
; 			(Why do you _! your * ?)))

; (setq *rule2* '(/ (I am (?1 so not) (*1 very) (!1 upset happy) about 
; 					my (!2 grades weight relationship))
; 				(Why are you ?1 *1 !1 about your !2 ?)))

; (setq *rule3* '(/ (!1 (I (!2 am was) (?3 $empty$) _!1)
; 					(I (!2 will would) (?3 be) _!1)
; 					(I (!2 have had) (?3 been) _!1))
; 				(I !2 not ?3 _!1)))

; (setq *rule4* '(/ ((!1 I ~ you we he she it yo) (!2 am ~ are is soy) (!3 from ~ de) _!1)
; 				(What is it like in _!1 ?)))

(defun print-hash-value (key value)
	(format t " ~a " value))

; checks if x is a member of list lst

(defun mem (x lst)
	(cond
		((null lst) nil)
		((equal (car lst) x) t)
		(t (mem x (cdr lst)))))

; check if rule is a rule

(defun is-rule (rule)
	(and (listp rule)
			(eql (list-length rule) 3)
			(eq (first rule) '/)
			(is-fact (second rule) nil)
			(or (is-fact (third rule) nil)
				(is-rule (third rule)))))

;check if fact is a fact. flag is nil if it is a fact from 
; a rule, it is true if it is a raw fact

(defun is-fact (fact flag)
	(or (and (eql (list-length fact) 2)
			(eq (first fact) 'not)
			(listp (second fact))
			(is-pos-fact (second fact) flag))
		(is-pos-fact fact flag)))

;checks if fact is a positive fact

(defun is-pos-fact (fact flag)
	(and (listp fact)
			(> (list-length fact) 0)
			(not (listp (first fact)))
			(not (variable? (first fact)))
			(check-arguments (cdr fact) flag)))

;checks to see if the arguments in a fact are of a 
;certain form

(defun check-arguments (arglist flag)
	(if arglist
		(let ((tok (first arglist)))
			(if (listp tok)
				nil
				(if flag
					(if (not (variable? tok))
						(check-arguments (cdr arglist) flag))
					(if (or (eq (variable? tok) '_!) (not (variable? tok)))
						(check-arguments (cdr arglist) flag)))))
		t))

;gets hash key of rule

(defun get-rule-key (rule)
	(get-fact-key (second rule)))

;gets hash key of fact

(defun get-fact-key (fact)
	(if (and (eql (list-length fact) 2)
			(listp (second fact)))
		(list 'not (first (second fact)))
		(first fact)))

;adds rule to a given hash

(defun add-rule (rule rulehash)
 	(let ((rulelist (gethash (get-rule-key rule) rulehash)))
 		;(if (not (member rule rulelist))
 			(setf (gethash (get-rule-key rule) rulehash) (cons rule rulelist))))


;adds fact to a given hash
(defun add-fact (fact facthash)
	(let ((factlist (gethash (get-fact-key fact) facthash)))
		;(if (not (member fact factlist))
			(setf (gethash (get-fact-key fact) facthash) (cons fact factlist))))

;fact list. I got the facts from the 173 project "family matters"

(setq *facts* (make-hash-table :test 'equal))
(setq *simplefacts* (make-hash-table :test 'equal))

(add-fact '(dog Rover) *simplefacts*)
(add-fact '(owns Bob Rover) *simplefacts*)

; (setq *fact3* '(male i))
; (setq *fact4* '(spouse i widow))
; (setq *fact5* '(spouse widow i))
; (setq *fact6* '(female widow))
; (setq *fact7* '(child redhair widow))
; (setq *fact8* '(female redhair))
; (setq *fact9* '(child i dad))
; (setq *fact10* '(male dad))
; (setq *fact11* '(spouse dad redhair))
; (setq *fact12* '(spouse redhair dad))
; (setq *fact13* '(child onrun dad))
; (setq *fact14* '(male onrun))
; (setq *fact15* '(child baby i))
; (setq *fact16* '(male baby))

(add-fact '(male i) *facts*)
(add-fact '(spouse i widow) *facts*)
(add-fact '(spouse widow i) *facts*)
(add-fact '(female widow) *facts*)
(add-fact '(child redhair widow) *facts*)
(add-fact '(female redhair) *facts*)
(add-fact '(child i dad) *facts*)
(add-fact '(male dad) *facts*)
(add-fact '(spouse dad redhair) *facts*)
(add-fact '(spouse redhair dad) *facts*)
(add-fact '(child onrun dad) *facts*)
(add-fact '(male onrun) *facts*)
(add-fact '(child baby i) *facts*)
(add-fact '(male baby) *facts*)

;rule list, also from family matters

(setq *rules* (make-hash-table :test 'equal))
(setq *simplerules* (make-hash-table :test 'equal))

(add-rule '(/ (dog _!x) (barks _!x)) *simplerules*)
(add-rule '(/ (dog _!x) (/ (owns _!y _!x) (adores _!x _!y))) *simplerules*)
(add-rule '(/ (dog _!x) (/ (owns _!y _!x) (/ (good-person _!y) (feeds _!y _!x)))) *simplerules*)
; (setq *rule1* )
; (setq *rule1* )
; (setq *rule1* )
; (setq *rule1* )
; (setq *rule1* )
; (setq *rule1* )
; (setq *rule1* )
; (setq *rule1* )
; (setq *rule1* )
; (setq *rule1* )
; (setq *rule1* )
; (setq *rule1* )
; (setq *rule1* )
; (setq *rule1* )

;(setq *rule4* nil)

; (add-rule *rule1* *rules*)
; (add-rule *rule2* *rules*)
; (add-rule *rule3* *rules*)
(add-rule '(/ (child _!c _!s) (/ (spouse _!s _!p) (stepchild _!c _!p))) *rules*)
(add-rule '(/ (stepchild _!c _!p) (not (inlawchild _!c _!p))) *rules*)
(add-rule '(/ (child _!c _!p) (not (inlawchild _!c _!p))) *rules*)
(add-rule '(/ (inlawchild _!s _!p) (/ (male _!s) (son_in_law _!s _!p))) *rules*)
(add-rule '(/ (inlawchild _!d _!p) (/ (female _!d) (daughter_in_law _!d _!p))) *rules*)
(add-rule '(/ (not (inlawchild _!s _!p)) (/ (spouse _!c _!s) (inlawchild _!c _!p))) *rules*)
(add-rule '(/ (inlawchild _!c _!p) (anychild _!c _!p)) *rules*)
(add-rule '(/ (child _!c _!p) (anychild _!c _!p)) *rules*)
(add-rule '(/ (stepchild _!c _!p) (anychild _!c _!p)) *rules*)
(add-rule '(/ (anychild _!d _!p) (/ (female _!d) (daughter _!d _!p))) *rules*)
(add-rule '(/ (anychild _!s _!p) (/ (male _!s) (son _!s _!p))) *rules*)
(add-rule '(/ (anychild _!c _!m) (/ (female _!m) (mother _!m _!c))) *rules*)
(add-rule '(/ (anychild _!c _!f) (/ (male _!f) (father _!f _!c))) *rules*)
(add-rule '(/ (anychild _!s _!p) (/ (anychild _!b _!p) (/ (male _!b) (brother _!b _!s)))) *rules*)
(add-rule '(/ (anychild _!c _!p) (/ (anychild _!s _!p) (/ (female _!s) (sister _!s _!c)))) *rules*)
(add-rule '(/ (anychild _!p _!g) (/ (anychild _!c _!p) (grandchild _!c _!g))) *rules*)
(add-rule '(/ (grandchild _!c _!m) (/ (female _!m) (grandmother _!m _!c))) *rules*)
(add-rule '(/ (grandchild _!c _!f) (/ (male _!f) (grandfather _!f _!c))) *rules*)
(add-rule '(/ (anychild _!c _!p) (/ (brother _!u _!p) (uncle _!u _!c))) *rules*)
(add-rule '(/ (anychild _!c _!p) (/ (sister _!a _!p) (aunt _!a _!c))) *rules*)
(add-rule '(/ (grandfather i i) (/ (grandchild i widow) (/ (grandmother widow i) 
			(/ (mother widow redhair) (/ (grandchild onrun i) (/ (brother baby redhair) 
			(/ (uncle baby i) (/ (brother baby dad) (/ (son_in_law dad i) (/ (mother redhair i) 
			(/ (daughter redhair i) (finished)))))))))))) *rules*)
;(setq *rules* (cons *rule1* (cons *rule2* (cons *rule3* (cons *rule4* nil)))))



(setq *verbose* t)

;performs forward-inferences

(defun forward-inferences (rule-table fact-table)
	(progn (setq new-rules nil)
		(setq new-facts nil)
		(forward-inferences-helper rule-table fact-table)
		(if *verbose* 
			(format t "~%~%New rules:~a~%~%New facts:~a" new-rules new-facts))))

;if going through all the rules and there are no new ones,
;end. else, repeat.

(defun forward-inferences-helper (rule-table fact-table)
	(if (forward-inferences-helper2 rule-table fact-table nil)
		(forward-inferences-helper rule-table fact-table)))

;loops through every fact, then checks the rules that have
;the same hash key as the fact

(defun forward-inferences-helper2 (rule-table fact-table flag)
	(progn
	(loop for factlist being the hash-values of fact-table
		using (hash-key key)
		do (loop for fact in factlist
			do (progn 
				;(format t "fact: ~a~%" fact)
				(loop for rule in (gethash key rule-table)
				do (progn
					;(format t "rule: ~a~%" rule)
					(let ((result (try-rule rule fact)))
						(setf flag (or flag (update-hashes rule-table fact-table result)))))))))
	flag))

;after matching rule with a fact, check if the result is
; a rule or a fact, and add to the relevant hash

(defun update-hashes (rule-table fact-table result)
	(if (is-rule result) 
		(if (not (mem result (gethash (get-rule-key result) rule-table)))
			(progn (add-rule result rule-table)
					(setq new-rules (cons result new-rules))
					(if *verbose*
						(format t "New rule: ~a~%" result))
					t))
		(if (is-fact result t)
			(if (not (mem result (gethash (get-fact-key result) fact-table)))
				(progn (add-fact result fact-table) 
						(setq new-facts (cons result new-facts))
						(if *verbose*
							(format t "New fact: ~a~%" result))
						t))
			(if result
				(format t "~a is not of the right form~%" result)))))

; (defun find-matches (rule-table fact-table rulelist factlist flag)
; 	(if factlist
; 		(let 
; 			)
; 	(let ((rulelist ()))))






;;Input: an S-expression S
;;
;;output: if S is an atom whose first characters
;;are _+, _!, _?, or _*, then output those two characters.
;;if S is an atom and its first 2 characters are not in this
;;list, (or if it has fewer than 2 characters),output nil. 
;;If S is a list, output nil.

;;Also, now recognizes variables starting with !,+,?,*.

;;allows us to tell when a token is a variable, and what
;;type it is., if it is.
(defun variable? (expr)
	(if (atom expr) 
		(let ((str (write-to-string expr)))
			(let ((substr1 (subseq str 0 1)))
				(if (or (string-equal substr1 "!")
						(string-equal substr1 "?")
						(string-equal substr1 "*")
						(string-equal substr1 "+"))
					(let ((out (intern substr1)))
						out)
					(if (>= (length str) 2) 
						(let ((substr2 (subseq str 0 2)))
							(if (or (string-equal substr2 "_!")
									(string-equal substr2 "_?")
									(string-equal substr2 "_*")
									(string-equal substr2 "_+"))
								(let ((out (intern substr2)))
									out)))))))))

;;Input: two S-expressions patt and expr
;;
;;output: if they match, then outputs what each variable
;;in patt matches to in expr. Handles making the hash table
;;as well as checking if the hash is empty at the end of our program.
(defun match-expr (patt expr)
	(progn  (setq table (make-hash-table :test 'eq))
			(if (match-expr-helper patt expr)
				(if (> (hash-table-count table) 0)
					(print-hash nil)
					t))))

;;Input: two S-expressions patt and expr
;;
;;basically an extension of above, this takes care of the case
;;that patt is just a single variable, either constrained or
;;unconstrained, not necessarily in a list.
(defun match-expr-helper (patt expr)
	(if (atom patt)
		(case (variable? patt)
			(_* (progn (setf (gethash patt table) (list expr)) t))
			(_+ (progn (setf (gethash patt table) (list expr)) t))
			(_! (progn (setf (gethash patt table) expr) t))
			(_? (progn (setf (gethash patt table) expr) t))
			(t (eq patt expr)))
		(case (variable? (first patt))
			(! (if (constrained_match patt expr)
					(progn (setf (gethash (first patt) table) expr) t)))
			(? (if (constrained_match patt expr)
					(progn (setf (gethash (first patt) table) expr) t)))
			(+ (if (constrained_match patt expr)
					(progn (setf (gethash (first patt) table) (list expr)) t)))
			(* (if (constrained_match patt expr)
					(progn (setf (gethash (first patt) table) (list expr)) t)))
			(t (if (listp expr) 
					(match-list patt expr))))))
		

;;Input: Two lists, patt and expr
;;
;;here is the meat of the program. This handles the case where
;;both patt and expr are lists, then looks at the first elmt
;;in patt, determines if it is a variable (see above), and handles
;;each case of each variable separately. If it is another symbol,
;;match-list tries to match it with the first symbol of expr. If that
;;fails, there can be no match.
;;If the first elmt of patt is a list, we recurse into the list
;;and then continue down the upper-level list after finishing the
;;lower-level one.
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
			(case (variable? (first tok))
				(! (if expr (!handler patt expr (first tok))))
				(? (?handler patt expr (first tok)))
				(+ (if expr (+handler patt expr (first tok) nil nil)))
				(* (*handler patt expr (first tok) nil))
				(t (if (listp (first expr)) 
						(and (match-list tok (first expr)) 
							(match-list (cdr patt) (cdr expr)))))))))


;;Input: the patterns in a constrained variable,
;;and an aggragate list.
;;Output: gets the "allowed" patterns in the constrained variable
(defun get_allowed (var agg)
	(if (or (not var) (string-equal (write-to-string (first var)) "~"))
		agg
		(get_allowed (cdr var) (cons (car var) agg))))

;;Similar to above, gets the "unallowed" patterns.
(defun get_unallowed (var agg record?)
	(if (not var)
		agg
		(if record? 
			(get_unallowed (cdr var) (cons (car var) agg) record?)
			(get_unallowed (cdr var) agg 
							(string-equal (write-to-string (first var)) "~")))))

;;Given a list of patterns, checkes to see if any of them
;;matches expr.
(defun check-match (lst expr)
	(if lst
		(if (not (match-expr-helper (first lst) expr))
			(check-match (cdr lst) expr)
			t)))

;;Using the above functions, checks to see if a given expr
;;matches a constrained variable 'patt'
(defun constrained_match (patt expr)
	(let ((allowed (reverse (get_allowed (cdr patt) nil))) 
		  (unallowed (reverse (get_unallowed (cdr patt) nil nil))))
		(let ((allowed_match? (check-match allowed expr))
				(unallowed_match? (check-match unallowed expr)))
			(if allowed
				(and allowed_match? (not unallowed_match?))
				(not unallowed_match?)))))

;;Useful for subst-bindings, this function
;;takes a list of bindings and a token, an returns
;;the expression or expression list that the token
;;has been bound to.
(defun get-value (bindings tok)
	(if bindings
		(if (eq (first (first bindings)) tok)
			(if (cdr (first bindings))
				(cdr (first bindings))
				"$$$EMPTY_STRING$$$") ;;tells subst-bindings that this token bound with the empty seq.
			(get-value (cdr bindings) tok))))

;;see below
(defun subst-bindings (bindings expr)
		(subst-bindings-helper bindings expr nil))


;;Input: A list of bindings, an expression, and aggregate
;;list.

;;Output: The original expression, but each variable is
;;replaced with its binding from 'bindings'.
(defun subst-bindings-helper (bindings expr agg)
	(if (listp expr)
		(if expr
			(if (listp (first expr))
				(subst-bindings-helper bindings (cdr expr)
					(append (list (subst-bindings-helper bindings (first expr) nil)) agg))
				(let ((binding (get-value bindings (first expr))))
					(if (and (variable? (first expr)) binding)
						(if (and (stringp binding) (string-equal binding "$$$EMPTY_STRING$$$"))
							(subst-bindings-helper bindings (cdr expr) agg)
							(subst-bindings-helper bindings (cdr expr) 
								(append (reverse (get-value bindings (first expr))) agg)))
						(subst-bindings-helper bindings (cdr expr) (cons (first expr) agg)))))
			(reverse agg))
		(if (variable? expr)
			(let ((binding (get-value bindings expr)))
				(if (and (stringp binding) 
						(string-equal binding "$$$EMPTY_STRING$$$"))
					(values)
					binding))
			expr)))

;;Input: a rule and an expression
;;Output: If the rule matches the expression, output
;;the result of the rule applied to the expr.
;;Otherwise, nil.
(defun try-rule (rule expr)
	(let ((match (match-expr (second rule) expr)))
		(if (eq match t)
			(third rule)
			(if match
				(subst-bindings match (third rule))))))

;;Input: A list of rules 'rules', and an expression
;;Output: if one of the rules matches, output the result
;;of that rule applied to the expr. If none match,
;;output nil.
(defun try-rules (rules expr)
	(if rules
		(let ((success (try-rule (first rules) expr)))
			(if success
				success
				(try-rules (cdr rules) expr)))))
		
;;handles the ! case, very similar to _!	
(defun !handler (patt expr tok)
	(if (constrained_match (first patt) (first expr))
		(progn (setf (gethash tok table) (first expr))
			(match-list (cdr patt) (cdr expr)))))

;;handles the ? case, again similar to _?
(defun ?handler (patt expr tok)
	(progn (setf (gethash tok table) "$$$EMPTY_STRING$$$")
		(if (not (match-list (cdr patt) expr))
			(!handler patt expr tok)
			t)))

;;handles the + case. Again kind of tricky. Basically,
;;it processes elements in expr one my one, matching
;;them to the first element in pattern (which we are assuming
;;is a + variable). If we match at least one element, we set
;;'flag' to true. If we do not match some element and we have
;;already matched at least one element, then we return
;;whatever happens when we try to match the rest of the pattern
;;list with the rest of the expr list.

;;In the case of a match, the code looks pretty similar to _+.
(defun +handler (patt expr tok agg flag)
	(if (constrained_match (first patt) (first expr))
		(progn (setf (gethash tok table) (cons (first expr) agg))
			(if (not (match-list (cdr patt) (cdr expr)))
				(if (cdr expr) 
					(+handler patt (cdr expr) tok (cons (first expr) agg) t))
				t))
		(if flag ;if we have already matched at least one token
			(match-list (cdr patt) expr))))

;;pretty similar to _*.
(defun *handler (patt expr tok agg)
	(progn (setf (gethash tok table) "$$$EMPTY_STRING$$$")
		(if (not (match-list (cdr patt) expr))
			(+handler patt expr tok nil nil)
			t)))


;;Input: Two lists, patt, expr and an atom tok
;;
;;handles the _! variable case, fairly straightforward
(defun _!handler (patt expr tok)
	;;tok = (first expr)
	(progn (setf (gethash tok table) (first expr))
		   (match-list (cdr patt) (cdr expr))))

;;Input: see above
;;
;;handles the _? variable case. To safe space, I reduced this
;;alogorithm to the _! handler
(defun _?handler (patt expr tok)
	;;tok = empty
	(progn (setf (gethash tok table) "$$$EMPTY_STRING$$$")
			(if (not (match-list (cdr patt) expr))
				(_!handler patt expr tok)
				t)))

;;Input: see above
;;
;;This one is the most complex, and requires explanation.
;;basically, it "guesses" that the _+ variable matches with 
;;just one element in expr. Then it "tests" that guess by 
;;running (match-list (cdr patt) (cdr expr)). If that does not match,
;;It "guesses" that the _+ variable matches with the first 
;;two elements. And so on and so forth. This is done recursively
;;until expr runs out of elmts, at which case we return nil if
;;no successful matches have been found. The important thing is
;;that it finds the FIRST one, meaning it takes the "furthest left"
;;property that was imposed in the project description.
(defun _+handler (patt expr tok agg)
	;;tok = agg
	(progn (setf (gethash tok table) (cons (first expr) agg))
			(if (not (match-list (cdr patt) (cdr expr)))
				(if (cdr expr) (_+handler patt (cdr expr) tok (cons (first expr) agg)))
				t)))

;;Input: see above
;;
;;_* handler is very similar to _? in the fact that it is reduced to 
;;_+ handler after first guessing that the variable matches with the
;;empty string. FYI, empty string was taken to be the string 
;;"$$$EMPTY_STRING$$$" just because that is a very unlikely string to 
;;be used in the program. This is technically a flaw, so please don't make
;;any of the elements in the expr expression to be "$$$EMPTY_STRING$$$".
(defun _*handler (patt expr tok)
	;;tok = empty
	(progn (setf (gethash tok table) "$$$EMPTY_STRING$$$")
			(if (not (match-list (cdr patt) expr))
				(_+handler patt expr tok nil)
				t)))


;;input: nil. just a "local" variable. Dunno Why I did it this way, I'm lazy.
;;Coulda done a let.
;;
;;simple print function. Had to do some hackery at the end to get the lists to 
;;come out in the right order for _* and _+, and also to appear as a "sequence
;;of elements" as opposed to a list, which is how I was storing them.
;;
;;BTW, what a weird loop function syntax, seriously. Almost looks like English.
(defun print-hash (agg)
	(loop for v being the hash-values of table 
		using (hash-key k) 
		collect (if (and (stringp v) (string-equal v "$$$EMPTY_STRING$$$"))
						(list k)
						(case (variable? k)
							(_* (mklist (cons k nil) (reverse v)))
							(_+ (mklist (cons k nil) (reverse v)))
							(* (mklist (cons k nil) (reverse v)))
							(+ (mklist (cons k nil) (reverse v)))
							(t (list k v))))))
;;Input: agg, which keeps an aggregate of the sequence so far,
;;and rest which keeps track of the elements we have yet to aggregate.
;;
;;just a function to help put the "sequence of elements" together for _+ and _*.
(defun mklist (agg rest)
	(if rest
		(mklist (cons (first rest) agg) (cdr rest))
		(reverse agg)))