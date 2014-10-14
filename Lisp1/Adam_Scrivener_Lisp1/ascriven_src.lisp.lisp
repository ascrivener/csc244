;;Input: an S-expression S
;;
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
;;that patt is an atom (which turns out to be an important case).
;;If patt is not an atom, sends info to match-list.
(defun match-expr-helper (patt expr)
	(if (atom patt)
		(case (variable? patt)
			(_* (progn (setf (gethash patt table) (list expr))) t)
			(_+ (progn (setf (gethash patt table) (list expr))) t)
			(_! (progn (setf (gethash patt table) expr)) t)
			(_? (progn (setf (gethash patt table) expr)) t)
			(t (eq patt expr)))
		(if (listp expr) (match-list patt expr))))

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
			(if (listp (first expr)) 
				(and (match-list tok (first expr)) 
					 (match-list (cdr patt) (cdr expr)))))))


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
							(t (list k v))))))

;;Input: agg, which keeps an aggregate of the sequence so far,
;;and rest which keeps track of the elements we have yet to aggregate.
;;
;;just a function to help put the "sequence of elements" together for _+ and _*.
(defun mklist (agg rest)
	(if rest
		(mklist (cons (first rest) agg) (cdr rest))
		(reverse agg)))