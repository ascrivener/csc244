CONTENTS: 
		Lisp1.lisp: the file containing the source code for the project.

USAGE: 
		If using Emacs, simply load the file and then start matching patterns! Simply type (match-expr patt expr), where patt and expr are S-expressions, and patt possibly contains atoms of the form '_![---] '_*[---] '_?[---] or '_+[---], where each atom of this form matches with expr as explained in the project description.

EXAMPLE:
		See the test file for examples.

APPROACH:
		My first idea for implementing this program was to have a hash table for the variables, so that we could easily store what expressions each of them matches with, and we can easily overwright/access them. So that was that.


		When I first began coding, I only implemented variables of the type _!. This was fairly simple. 

		Then I added _?. This is where things got tricky. If _? matches with the empty sequence successfully, I should stop right there and not consider the case where _? matches 1 S-expression. This means that I should consider two worlds: one where the rest of the sequence matches fine when _? matches with the empty sequence, and one where it does not. I finally settled on using the power of recursion to solve this. I simply ask my program if (cdr patt) matched with (expr) worked. If they did, stop right there, bind _? with empty string. If not, consider whether (cdr patt) matches with (cdr expr). If so, then _? should be binded with (car expr). This type of thought process allowed me to handle the _? case fairly smoothly.

		The biggest issue in this program was the _+ case (and by extension, _*).This one would have to consider not only if the variable matches with the empty string, but if it matches with ANY number of S-expressions, taking the first one it found to satisfy the "furthest left" property. Honestly this one just took a bunch of thinking. The code itself looks pretty tame but a lot of work went into it. Basically, the handler calls itself recursively until it runs out of all possible bindings or finds one that works. It makes calls to (match-expr) to verify if a possible binding works. The details of the program get kind of complex, so I leave it to the grader to make sense of them what they will. Oh also the agg variable keeps track of the S-expressions we have already bound.

		The _* function is a simple extension of _+.

		match-list and match-expr-helper are fairly straightforward in comparison. Those did not take much time to figure out. They are explained pretty well in the code.

		Getting the hash to print out nicely was kind of a pain. The looping is awkward, and I do some weird things using (reverse) to get the lists for _+ and _* to look like a "sequence of elements" rather than a list. Not really anything clever there, just awkward code.

		Other than that, everything else is pretty evident. To be able to describe every little bit of my code would be difficult and perhaps even more confusing than giving a rough overview. If you want, pour over it yourself. That may be best.