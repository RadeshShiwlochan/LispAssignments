(defun rec_length (L)
   (if(endp L)
        0
        (let ((X(rec_length(cdr L))))
               (+ X 1))))

(defun sum-list (L)
	(if(endp L)
		0
		(let((X(sum-list(cdr L))))
			(+ (car L) X))))

(defun minus-list (L)
	(if(endp L)
		0
		(let((X(minus-list(cdr L))))
			(- (car L) X))))

(defun neg-num (L)
	(if(endp L)
		'()
		(let((X(neg-num(cdr L))))
			(if(minusp (car L)) 
				(cons (car L) X) X))))


(defun inc-list (L num)
	(if (endp L) 
		'()
		(let((X(inc-list(cdr L) num)))
			(cons (+ (car L) num) X))))


(defun insert (N L)
	(if(endp L)
		(cons N L)
		(let((X(insert N (cdr L))))
			(if( < (car L) N)
				(cons (car L) X)
				    (cons N L)))))

(defun isort (L)
	(if(endp L)
		'Nil
		(let((X(isort (cdr L))))
			(insert(car L) X))))

(defun split-list (L)
	(if(endp L)
		'(() ())  
		(let((X(split-list(cdr L))))
			(append(list(cons(car L) (second X))) (list(car X))))))


(defun partition (L P)
   (if (endp L)
      '(()())
      (let ((X (partition (cdr L) P)))
         (if(< (car L) P) (list (cons(car L)(car X)) (cadr X))
               (list (car x)(cons (car L)(cadr X)))))))

(defun POS (E L)
	(cond ((endp L) 0)
		  ((equal E (car L)) 1)
		  (T (let ((x (POS E (cdr L))))
		  	(if(zerop x) 0
		  		(+ 1 x))))))

(defun split-num (N) 
	(if(zerop N)
		'((0) ())
		(let((x (split-num(- N 1))))
			(if(evenp N) (list(cons N (car x)) (cadr x))
				  (list(car x) (cons N (cadr x)))))))


; Solution to Problem 10
(defun set-union (set1 set2)
   (if (endp set1)
       set2
       (let ((x (set-union (cdr set1) set2)))
          (if (member (car set1) x)
              x
              (cons (car set1) x)))))

; Solution to Problem 11
(defun set-remove (E set1)
   (if (endp set1)
       '()
       (let ((x (set-remove E (cdr set1))))
          (if (or (member (car set1) x) (eq (car set1) E))
              x
              (cons (car set1) x)))))


(defun set-excl-union (set1 set2)
   (if (endp set1)
       set2
       (let ((x (set-excl-union (cdr set1) set2)))
          (if (member (car set1) x)
              (set-remove (car set1) x)
              (cons (car set1) x)))))

;Solution to Problem 13
(defun singletons (N)
   (if (endp N)
       '()
       (let ((x (singletons (cdr N))))
          (if (member (car N) x)
              (set-remove (car N) x)
              (set-remove (car N) N)))))
















