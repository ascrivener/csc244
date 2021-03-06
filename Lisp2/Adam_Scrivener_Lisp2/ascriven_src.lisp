;;(load "C:/Users/Adam/Code/csc244/Lisp2/Adam_Scrivener_Lisp2/ascriven_src.lisp")

(setq *rule1* '(/ (I _! (! my the)
			(* (!1 present stupid new) (!2 job boss room-mate)))
			(Why do you _! your * ?)))

(setq *rule2* '(/ (I am (?1 so not) (*1 very) (!1 upset happy) about 
					my (!2 grades weight relationship))
				(Why are you *1 !1 about your !2 ?)))

(setq *rule3* '(/ (!1 (I (!2 am was) (?3 $empty$) _!1)
					(I (!2 will would) (?3 be) _!1)
					(I (!2 have had) (?3 been) _!1))
				(I !2 not ?3 _!1)))

(setq *rule4* '(/ ((!1 I ~ you we he she it yo) (!2 am ~ are is soy) (!3 from ~ de) _!1)
				(What is it like in _!1 ?)))

(setq *rules* (cons *rule1* (cons *rule2* (cons *rule3* (cons *rule4* nil)))))

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
		(if match
			(subst-bindings match (third rule)))))

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