(define myVar 3)
(define triple (lambda (n) (* n 3)))
(define (cube n) (* n n n))
(quote ())
(quote "hello")
(quote (car (list (triple myVar) 5 10)))
myVar
(set! myVar #t)
myVar
(set! myVar 11)
myVar
(begin myVar (cube 4))
(let ((var 7)) (set! var 0) var)
(if (< 5 3) "yes" "no")
((if #t - /) 2 9)
