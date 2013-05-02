(define (make-counter)
  (let ((count 0))
    (lambda ()
      (set! count (+ count 1))
      count)))

(define c (make-counter))

(c)
(c)
(c)

(define (add a)
  (lambda (b)
    (+ a b)))

(define add3 (add 3))
(add3 4)
