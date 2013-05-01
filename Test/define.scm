(define var 5)
(define (square x) (* x x))

(define (factorial n)
  (if (= n 0)
      1
      (* n (factorial (- n 1)))))

(define (fib n)
  (if (<= n 1)
      1
      (+ (fib (- n 1)) (fib (- n 2)))))

(define (mul3 x y z) (* x y z))
(define compose (lambda (f g) (lambda (x) (f (g x)))))
(define repeat (lambda (f) (compose f f)))
(define abs (lambda (n) ((if (> n 0) + -) 0 n)))

(define combine (lambda (f)
  (lambda (x y)
      (if (null? x) (quote ())
          (f (list (car x) (car y))
             ((combine f) (cdr x) (cdr y)))))))

(define zip (combine cons))
(define take (lambda (n seq) (if (<= n 0) (quote ()) (cons (car seq) (take (- n 1) (cdr seq))))))
(define drop (lambda (n seq) (if (<= n 0) seq (drop (- n 1) (cdr seq)))))
(define (map f seq) (if (null? seq) (quote ()) (cons (f (car seq)) (map f (cdr seq)))))

(define Y
 (lambda (X)
  ((lambda (procedure)
     (X (lambda (arg) ((procedure procedure) arg))))
   (lambda (procedure)
     (X (lambda (arg) ((procedure procedure) arg)))))))

(define F*
 (lambda (func-arg)
  (lambda (n)
    (if (= n 0)
        1
        (* n (func-arg (- n 1)))))))

(define fact (Y F*))

var
(square 10)
(factorial 20)
(fib 11)
(mul3 5 9 2)
((compose list square) 3)
((repeat square) 5)
(list (abs -3) (abs 0) (abs 3))
(take 2 (map square (list 11 7 0 3 9)))
(drop 5 (list 1 2 3 4 5 6 7 8 9 10))
(fact 8)
