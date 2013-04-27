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

(define (mul3 x y z)
  (* x y z))
