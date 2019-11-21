(setq ext:*warn-on-redefinition* nil)

(defun reverseHelper(a b)
	(if a
	(reverseHelper (cdr a) (cons (car a) b))
	b
	))
(defun reverse(a)
	(reverseHelper a ())
	)
(defun map(f a)
	(if a
	(cons (funcall f (car a)) (map f (cdr a)))
	NIL
	))
(defun alreadyExist(a b)
	(if a
		(if (eql (car a) b)
			T
			(alreadyExist (cdr a) b)
			)
		NIL
	)
	)
(defun removeduplicateHelper(l r)
	(if r
		(if(alreadyExist l (car r))
		  (removeduplicateHelper l (cdr r))
		  (removeduplicateHelper (cons (car r) l) (cdr r))
		)
		l
    ))
(defun nub(l)
	(reverse (removeduplicateHelper NIL l))
	)
(defun fold(initial f l)
	(if l
		(fold (funcall f initial (car l)) f (cdr l))
		initial
		)
	)
(defun filter(f a)
	(if a
		(if (funcall f (car a))
			(cons (car a) (filer f (cdr a)))
			(filter f (cdr a))
			)
		NIL
		)
	)
(defun merge(a b)
	(if a
		(if b
			(if (< (car a) (car b))
				(cons (car a) (merge (cdr a) b))
				(cons (car b) (merge a (cdr b)))
			)
			(cons (car a) (merge (cdr a) b))
		)
		(if b
			(cons (car b) (merge a (cdr b)))
			NIL
			)
	))
(defun addtoend(a l)
	(reverse (cons a (reverse l)))
	)

(defun indexof(a l)
    (if l
        (if (eql a (car l))
            0
            (if (> (indexof a (cdr l)) -1)
                (+ 1 (indexof a (cdr l)))
                -1
            )
        )
        -1
    ))
(defun removeall(a l)
	(if l
		(if (eql a (car l))
			(removeall a (cdr l))
			(cons (car l) (removeall a (cdr l)))
			)
		NIL
		)
	)
(defun member(a l)
    (if l
        (if (eql a (car l))
            T
            (member a (cdr l))
        )
        NIL
    ))
(defun insert(a l)
	(if (alreadyExist l a)
		l
		(cons a l)
		)
	)
(defun intersection(a b)
    (if a
        (if (alreadyExist b (car a))
            (cons (car a) (intersection (cdr a) b))
            (intersection (cdr a) b)
        )
    ))
(defun union(a b)
    (if a
        (union (cdr a) (insert (car a) b))
        b
    ))
(defun difference(a b)
    (if a
        (if (alreadyExist b (car a))
            (difference (cdr a) b)
            (cons (car a) (difference (cdr a) b))
        )
        NIL
    ))
(defun symdiff(a b)
    (union (difference a b) (difference b a)))
(defun subsetp(a b)
	(if a
	    (if (alreadyExist b (car a))
		(subsetp (cdr a) b)
		NIL
		)
	T
	))
(defun supersetp(a b)
    (subsetp b a))
(defun cardinality(a)
    (if a
        (+ 1 (cardinality (cdr a)))
        0
    ))
(defun addtolist(a l)
    (if l
        (cons (cons a (car l)) (addtolist a (cdr l)))
        NIL
    ))
(defun powerset(a)
    (if a
        (append (addtolist (car a) (powerset (cdr a))) (powerset (cdr a)))
        '(())
    ))
(defun abs(a)
    (if (>= a 0)
        a
        (- 0 a)
    ))
(defun factorial (a) 
	(if (eql a 0)
		1
		(* a (factorial (- a 1)))
		)
	)
(defun right-tri(a b c)
    (eql (+ (* a a) (* b b)) (* c c)))
(defun gcd(a b)
    (if (> a b)
        (if (eql b 0)
            a
            (gcd b (- a b))
        )
        (if (eql a 0)
            b
            (gcd a (- b a))
        )
    ))

(defun lcm(a b)
    (/ (* a b) (gcd a b)))

(defun nth-fibo(a)
    (if (eql a 0)
        0
        (if (eql a 1)
            1
            (+ (nth-fibo (- a 1)) (nth-fibo (- a 2)))
        )
    ))
(defun recprime(n i)
    (if (> (* i i) n)
        T
        (if (eql (gcd n i) i)
            NIL
            (recprime n (+ 1 i))
        )
    ))

(defun primep(a)
    (recprime a 2))

(defun recnprime(a i)
    (if (eql a 0)
        (- i 1)
        (if (primep i)
            (recnprime (- a 1) (+ i 1))
            (recnprime a (+ i 1))
        )
    ))

(defun nth-prime(a)
    (recnprime a 2))
(defun div(a b)
    (eql (gcd a b) b))

(defun recsumfactors(a sum factor)
    (if (eql a factor)
        sum
        (if (div a factor)
            (recsumfactors a (+ sum factor) (+ factor 1))
            (recsumfactors a sum (+ factor 1))
        )
    ))

(defun sumfactors(a)
    (recsumfactors a 0 1))

(defun perfectp(a)
    (eql (sumfactors a) a))

(defun abundantp(a)
    (> (sumfactors a) a))

(defun deficientp(a)
    (< (sumfactors a) a))
(defun repl(f a)
	(if a
		(format T "~A -> ~A~%" (cons f a) (apply f a))
		T
	))
(defun loop-repl(f)
	(format t "Enter a list of arguments for ~A (NIL to stop):" f)
	(finish-output)
	(if(IGNORE-ERRORS(repl f (read)))
		NIL
		(loop-repl f)
		)
	)

(princ "***")
(terpri)
(princ "*** CSC 173 Computation & Formal Systems")
(terpri)
(princ "*** Project 3: Functional Programming in Lisp")
(terpri)
(princ "*** Zihan Xie and Tianyi Li")
(terpri)
(princ "***")
(terpri)
(terpri)
(princ "*** List Functions:")
(terpri)
(terpri)
(format t "APPEND (1 3 X A) (4 2 B)) => ~A~%" (append '(1 3 x A) '(4 2 B)))
(loop-repl 'append)
(format t "reverse (A B C D) => ~A~%" (reverse '(a b c d)))
(loop-repl 'reverse)
(defun add3 (x) (+ 3 x))
(format t "MAP add3 (1 2 3 4) => ~A~%" (map 'add3 '(1 2 3 4)))
(loop-repl 'map)
(format t "nub ’(1 1 2 4 1 2 5) => ~A~%" (nub '(1 1 2 4 1 2 5)))
(loop-repl 'nub)
(format t "fold 10 ’- ’(1 3 2) => ~A~%" (fold 10 '- '(1 3 2)))
(loop-repl 'fold)
(defun lessthan3 (x) (< x 3))
(format t "filter ’lessthan3 ’(1 4 5 2 1 6) => ~A~%" (filter 'lessthan3 '(1 4 5 2 1 6)))
(loop-repl 'filter)
(format t "merge ’(1 3 4 7) ’(2 3 6) => ~A~%" (merge '(1 3 4 7) '(2 3 6)))
(loop-repl 'merge)
(format t "addtoend ’D ’(A B C) => ~A~%" (addtoend 'd '(a b c)))
(loop-repl 'addtoend)
(format t "(indexof ’A ’(B C A D) => ~A~%" (indexof 'A '(B C A D)))
(format t "(indexof ’A ’(B C D F) => ~A~%" (indexof 'A '(B C D F)))
(loop-repl 'indexof)
(format t "remove-all ’A ’(B A C A A D A) => ~A~% "(removeall 'a '(b a c a a d a)))
(loop-repl 'removeall)

(terpri)
(princ "*** Set functions")
(terpri)
(format t "(member ’A ’(B C A D)) => ~A~%" (member 'a '(b c a d)))
(format t "(member ’Z ’(B C A D)) => ~A~%" (member 'z '(b c a d)))
(loop-repl 'member)
(format t "(insert ’A ’(B C D)) => ~A~%" (insert 'a '(b c d)))
(format t "(insert ’A ’(A B C D)) => ~A~%" (insert 'a '(a b c d)))
(loop-repl 'insert)
(format t "(intersection ’(A B C) ’(A C D)) => ~A~%" (intersection '(a b c) '(a c d)))
(loop-repl 'intersection)
(format t "(union ’(A B C)’(A C D)) => ~A~%" (union '(a b c) '(a c d)))
(loop-repl 'union)
(format t "(difference ’(A B C) ’(A C D)) => ~A~%" (difference '(a b c)'(a c d)))
(format t "(difference ’(A C D) ’(A B C)) => ~A~%" (difference '(a c d) '(a b c)))
(loop-repl 'difference)
(format t "(symdiff ’(A B C) ’(A C D)) => ~A~%" (symdiff '(a b c) '(a c d)))
(loop-repl 'symdiff)
(format t "(subsetp ’(A B) ’(A B C D)) => ~A~%" (subsetp '(a b) '(a b c d)))
(loop-repl 'subsetp)
(format t "(supersetp ’(A B C D) ’(A B)) => ~A~%" (supersetp '(a b c d) '(a b)))
(loop-repl 'supersetp)
(format t "(cardinality ’(A B C)) => ~A~%" (cardinality '(a b c)))
(loop-repl 'cardinality)
(format t "(powerset ’()) => ~A~%"(powerset '()))
(format t "(powerset ’(A B C)) => ~A~%" (powerset '(a b c)))
(loop-repl 'powerset)

(terpri)
(princ "*** Math Functions:")
(terpri)

(format t "(abs 7) => ~A~%" (abs 7))
(format t "(abs -7) => ~A~%" (abs -7))
(loop-repl 'abs)
(format t "(factorial 5) => ~A~%" (factorial 5))
(loop-repl 'factorial)
(format t "(right-tri 3 4 5) => ~A~%" (right-tri 3 4 5))
(format t "(right-tri 1 2 3) => ~A~%" (right-tri 1 2 3))
(loop-repl 'right-tri)
(format t "(gcd 8 12) => ~A~%" (gcd 8 12))
(loop-repl 'gcd)
(format t "(lcm 4 6) => ~A~%" (lcm 4 6))
(loop-repl 'lcm)
(format t "(nth-fibo 6) => ~A~%" (nth-fibo 6))
(format t "(nth-fibo 10) => ~A~%" (nth-fibo 10))
(loop-repl 'nth-fibo)
(format t "(primep 5) -> ~A~%" (primep 5))
(format t "(primep 6) -> ~A~%" (primep 6))
(loop-repl 'primep)
(format t "(nth-prime 6) -> ~A~%" (nth-prime 6))
(format t "(nth-prime 26) -> ~A~%" (nth-prime 26))
(loop-repl 'nth-prime)

(terpri)
(princ "*** Required Fucntion:")
(terpri)
(format t "(perfectp 5) -> ~A~%" (perfectp 5))
(format t "(perfectp 6) -> ~A~%" (perfectp 6))
(loop-repl 'perfectp)

(format t "(abundantp 5) -> ~A~%" (abundantp 5))
(format t "(abundantp 12) -> ~A~%" (abundantp 12))
(loop-repl 'abundantp)

(format t "(deficientp 5) -> ~A~%" (deficientp 5))
(format t "(deficientp 12) -> ~A~%" (deficientp 12))
(loop-repl 'deficientp)