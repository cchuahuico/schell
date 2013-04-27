(define var 5)
(define (square x) (* x x))

(define (factorial n)
  (if (= n 0)
      1
      (factorial (- n 1))))

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
